#!/usr/bin/env Rscript

# Explore vectorized built-in operations in comparison_plan_apply().
# Run from the repository root:
# Rscript r/sandbox/benchmark_comparison_plan_fast_path.R

required <- c("bench", "collapse", "pkgload")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
    stop("Install required packages: ", paste(missing, collapse = ", "), call. = FALSE)
}

pkgload::load_all("r", quiet = TRUE)

n <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_N", "50000"))
iterations <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_ITERATIONS", "30"))

capture_plan <- function(fun) {
    setting <- "marginaleffects_capture_simulation_replay"
    old <- marginaleffects:::settings_get(setting)
    on.exit({
        if (is.null(old)) {
            marginaleffects:::settings_rm(setting)
        } else {
            marginaleffects:::settings_set(setting, old)
        }
    })
    marginaleffects:::settings_set(setting, TRUE)
    out <- fun()
    mfx <- attr(out, "marginaleffects")
    attr(mfx, "marginaleffects_simulation_replay")$plan
}

comparison_fast_path_metadata <- function(plan) {
    keys <- vapply(plan$groups, function(g) {
        if (is.null(g$fun_key)) "" else g$fun_key
    }, character(1))
    scalar <- vapply(plan$groups, function(g) g$scalar, logical(1))
    supported <- c("difference", "ratio", "lnratio", "lift")

    eligible <-
        !any(scalar) &&
        length(unique(keys)) == 1L &&
        keys[[1]] %in% supported &&
        all(vapply(
            plan$groups,
            function(g) identical(g$idx, g$out_idx),
            logical(1)
        ))

    if (eligible) keys[[1]] else NULL
}

comparison_plan_apply_fast <- function(plan, hi, lo, y = NULL) {
    key <- plan$fast_path_key
    if (is.null(key)) {
        return(marginaleffects:::comparison_plan_apply(plan, hi, lo, y))
    }

    if (!is.null(plan$na_keep)) {
        hi <- hi[plan$na_keep]
        lo <- lo[plan$na_keep]
    }
    if (!is.null(plan$perm)) {
        hi <- hi[plan$perm]
        lo <- lo[plan$perm]
    }

    estimate <- switch(
        key,
        difference = hi - lo,
        ratio = hi / lo,
        lnratio = log(hi / lo),
        lift = (hi - lo) / lo
    )
    if (!is.null(plan$est_keep)) {
        estimate <- estimate[plan$est_keep]
    }
    marginaleffects:::apply_plan_aggregation_and_hypothesis(
        estimate,
        plan$agg,
        plan$hyp
    )
}

set.seed(1024)
data <- data.frame(
    y = stats::rnorm(n),
    x1 = stats::rnorm(n),
    x2 = stats::rnorm(n),
    groupid = sample(letters[1:10], n, replace = TRUE)
)
model <- stats::lm(y ~ x1 * x2 + groupid, data = data)

cases <- list(
    difference = function() comparisons(
        model,
        variables = c("x1", "x2"),
        vcov = FALSE
    ),
    ratio = function() comparisons(
        model,
        variables = c("x1", "x2"),
        comparison = "ratio",
        vcov = FALSE
    ),
    average_by_group = function() avg_comparisons(
        model,
        variables = c("x1", "x2"),
        by = "groupid",
        vcov = FALSE
    )
)

results <- lapply(names(cases), function(case) {
    plan <- capture_plan(cases[[case]])
    plan$fast_path_key <- comparison_fast_path_metadata(plan)
    predictions <- marginaleffects:::comparison_plan_predict(plan, model)

    current <- marginaleffects:::comparison_plan_apply(
        plan,
        predictions$hi,
        predictions$lo,
        predictions$or
    )
    fast <- comparison_plan_apply_fast(
        plan,
        predictions$hi,
        predictions$lo,
        predictions$or
    )
    stopifnot(isTRUE(all.equal(current, fast, tolerance = 1e-12)))

    timings <- bench::mark(
        current = marginaleffects:::comparison_plan_apply(
            plan,
            predictions$hi,
            predictions$lo,
            predictions$or
        ),
        fast = comparison_plan_apply_fast(
            plan,
            predictions$hi,
            predictions$lo,
            predictions$or
        ),
        iterations = iterations,
        check = TRUE,
        memory = FALSE
    )

    data.frame(
        case = case,
        fast_path = if (is.null(plan$fast_path_key)) "fallback" else plan$fast_path_key,
        current_seconds = as.numeric(timings$median[[1]]),
        fast_seconds = as.numeric(timings$median[[2]]),
        speedup = as.numeric(timings$median[[1]]) /
            as.numeric(timings$median[[2]])
    )
})

print(data.table::rbindlist(results))

# Grouped averages: compare the current loop with direct dispatch, collapse,
# and data.table. A precomputed collapse::GRP avoids rebuilding the grouping
# structure during every replay.
average_plan <- capture_plan(cases$average_by_group)
average_predictions <- marginaleffects:::comparison_plan_predict(average_plan, model)
group_id <- integer(sum(lengths(lapply(average_plan$groups, `[[`, "idx"))))
for (i in seq_along(average_plan$groups)) {
    group_id[average_plan$groups[[i]]$idx] <- i
}
collapse_group <- collapse::GRP(group_id)

prepare_average_predictions <- function() {
    hi <- average_predictions$hi
    lo <- average_predictions$lo
    if (!is.null(average_plan$perm)) {
        hi <- hi[average_plan$perm]
        lo <- lo[average_plan$perm]
    }
    list(hi = hi, lo = lo)
}

average_direct <- function() {
    prediction <- prepare_average_predictions()
    vapply(average_plan$groups, function(group) {
        mean(prediction$hi[group$idx] - prediction$lo[group$idx])
    }, numeric(1))
}

average_collapse <- function() {
    prediction <- prepare_average_predictions()
    unname(collapse::fmean(
        prediction$hi - prediction$lo,
        collapse_group,
        na.rm = FALSE
    ))
}

average_data_table <- function() {
    prediction <- prepare_average_predictions()
    data.table::data.table(
        value = prediction$hi - prediction$lo,
        group_id = group_id
    )[, mean(value), by = group_id][["V1"]]
}

average_current <- marginaleffects:::comparison_plan_apply(
    average_plan,
    average_predictions$hi,
    average_predictions$lo,
    average_predictions$or
)
stopifnot(
    isTRUE(all.equal(average_current, average_direct(), tolerance = 1e-12)),
    isTRUE(all.equal(average_current, average_collapse(), tolerance = 1e-12)),
    isTRUE(all.equal(average_current, average_data_table(), tolerance = 1e-12))
)

cat("\nGrouped average kernels:\n")
print(bench::mark(
    current = marginaleffects:::comparison_plan_apply(
        average_plan,
        average_predictions$hi,
        average_predictions$lo,
        average_predictions$or
    ),
    direct = average_direct(),
    collapse = average_collapse(),
    data_table = average_data_table(),
    iterations = iterations,
    check = TRUE,
    memory = FALSE
)[, c("expression", "median")])
