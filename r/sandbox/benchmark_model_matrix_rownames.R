# Benchmark the effect of removing cached model-matrix row names in the actual
# marginaleffects prediction and comparison paths.

library(marginaleffects)

set.seed(48103)
n <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_N", "1000000"))
repetitions <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_REPS", "5"))

train <- data.frame(
    y = rnorm(2000),
    x1 = rnorm(2000),
    x2 = rnorm(2000),
    x3 = rnorm(2000),
    x4 = rnorm(2000),
    x5 = rnorm(2000)
)
model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = train)
newdata <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    x3 = rnorm(n),
    x4 = rnorm(n),
    x5 = rnorm(n)
)

installed_mode <- Sys.getenv("MARGINALEFFECTS_BENCH_INSTALLED_MODE", "")
if (nzchar(installed_mode)) {
    benchmark_installed <- function(fun, reps = repetitions) {
        invisible(fun())
        elapsed <- numeric(reps)
        for (i in seq_len(reps)) {
            gc(FALSE)
            elapsed[i] <- unname(system.time(invisible(fun()))[["elapsed"]])
        }
        elapsed
    }

    probe <- predictions(model, newdata = newdata[1:10, , drop = FALSE])
    probe_matrix <- attr(
        components(probe, "newdata"),
        "marginaleffects_model_matrix"
    )
    prediction_times <- benchmark_installed(function() {
        predictions(model, newdata = newdata)
    })
    comparison_times <- benchmark_installed(function() {
        avg_comparisons(model, newdata = newdata, variables = "x1")
    })
    cat("Mode:", installed_mode, "\n")
    cat("Cached row names are NULL:", is.null(rownames(probe_matrix)), "\n")
    cat("predictions times:", paste(prediction_times, collapse = " "), "\n")
    cat("predictions median:", median(prediction_times), "\n")
    cat("avg_comparisons times:", paste(comparison_times, collapse = " "), "\n")
    cat("avg_comparisons median:", median(comparison_times), "\n")
    quit(save = "no")
}

MM_named <- marginaleffects:::get_model_matrix(model, newdata)
cold_mode <- Sys.getenv("MARGINALEFFECTS_BENCH_COLD_MODE", "")
if (cold_mode %in% c("named", "plain")) {
    if (cold_mode == "plain") {
        rownames(MM_named) <- NULL
    }
    nd_cold <- newdata
    attr(nd_cold, "marginaleffects_model_matrix") <- MM_named
    gc(FALSE)
    cold_elapsed <- unname(system.time(invisible(marginaleffects:::get_predict(
        model,
        newdata = nd_cold,
        type = "response"
    )))[["elapsed"]])
    cat(cold_mode, cold_elapsed, "\n")
    quit(save = "no")
}
MM_plain <- marginaleffects:::get_model_matrix(model, newdata)
rownames(MM_plain) <- NULL
beta <- coef(model)

stopifnot(
    identical(dim(MM_named), dim(MM_plain)),
    identical(colnames(MM_named), colnames(MM_plain)),
    identical(unname(MM_named), unname(MM_plain)),
    is.null(rownames(MM_plain))
)

cat("Rows:", n, " Columns:", ncol(MM_named), "\n")
cat("object.size named:", format(object.size(MM_named), units = "MiB"), "\n")
cat("object.size plain:", format(object.size(MM_plain), units = "MiB"), "\n")
if (requireNamespace("lobstr", quietly = TRUE)) {
    cat("lobstr::obj_size named:", format(lobstr::obj_size(MM_named)), "\n")
    cat("lobstr::obj_size plain:", format(lobstr::obj_size(MM_plain)), "\n")
    cat(
        "lobstr::obj_size row names:",
        format(lobstr::obj_size(rownames(MM_named))),
        "\n"
    )
}

benchmark_pair <- function(named, plain, reps = repetitions) {
    elapsed <- list(named = numeric(), plain = numeric())
    modes <- sample(rep(c("named", "plain"), reps))
    for (mode in modes) {
        gc(FALSE)
        elapsed[[mode]] <- c(
            elapsed[[mode]],
            unname(system.time(invisible(if (mode == "named") named() else plain()))[["elapsed"]])
        )
    }
    list(
        times = elapsed,
        summary = c(
            named = median(elapsed$named),
            plain = median(elapsed$plain),
            speedup = median(elapsed$named) / median(elapsed$plain)
        )
    )
}

raw <- benchmark_pair(
    named = function() drop(MM_named %*% beta),
    plain = function() drop(MM_plain %*% beta)
)
cat("\nRaw matrix multiplication\n")
print(raw)

matrix_difference <- benchmark_pair(
    named = function() MM_named - MM_named,
    plain = function() MM_plain - MM_plain
)
cat("\nAnalytic X_hi - X_lo kernel\n")
print(matrix_difference)

row_derivative <- runif(n)
response_derivative <- benchmark_pair(
    named = function() MM_named * row_derivative,
    plain = function() MM_plain * row_derivative
)
cat("\nResponse-scale row derivative kernel\n")
print(response_derivative)

nd_named <- newdata
attr(nd_named, "marginaleffects_model_matrix") <- MM_named
nd_plain <- newdata
attr(nd_plain, "marginaleffects_model_matrix") <- MM_plain

cached_predict <- benchmark_pair(
    named = function() marginaleffects:::get_predict(
        model,
        newdata = nd_named,
        type = "response"
    ),
    plain = function() marginaleffects:::get_predict(
        model,
        newdata = nd_plain,
        type = "response"
    )
)
cat("\nCached get_predict.lm path\n")
print(cached_predict)

original_get_model_matrix_lm <- get(
    "get_model_matrix.lm",
    envir = asNamespace("marginaleffects")
)
stripped_get_model_matrix_lm <- function(...) {
    MM <- original_get_model_matrix_lm(...)
    rownames(MM) <- NULL
    MM
}
set_mode <- function(strip) {
    assignInNamespace(
        "get_model_matrix.lm",
        if (strip) stripped_get_model_matrix_lm else original_get_model_matrix_lm,
        ns = "marginaleffects"
    )
}
on.exit(set_mode(FALSE), add = TRUE)

run_predictions <- function(strip) {
    set_mode(strip)
    predictions(model, newdata = newdata)
}
run_comparisons <- function(strip) {
    set_mode(strip)
    avg_comparisons(model, newdata = newdata, variables = "x1")
}

set_mode(FALSE)
pred_named <- predictions(model, newdata = newdata)
set_mode(TRUE)
pred_plain <- predictions(model, newdata = newdata)
stopifnot(isTRUE(all.equal(
    pred_named$estimate,
    pred_plain$estimate,
    check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
    pred_named$std.error,
    pred_plain$std.error,
    check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
    components(pred_named, "jacobian"),
    components(pred_plain, "jacobian"),
    check.attributes = FALSE
)))
rm(pred_named, pred_plain)
gc(FALSE)

set_mode(FALSE)
cmp_named <- avg_comparisons(model, newdata = newdata, variables = "x1")
set_mode(TRUE)
cmp_plain <- avg_comparisons(model, newdata = newdata, variables = "x1")
stopifnot(isTRUE(all.equal(
    cmp_named$estimate,
    cmp_plain$estimate,
    check.attributes = FALSE
)))
stopifnot(isTRUE(all.equal(
    components(cmp_named, "jacobian"),
    components(cmp_plain, "jacobian"),
    check.attributes = FALSE
)))
rm(cmp_named, cmp_plain)
gc(FALSE)

full_predictions <- benchmark_pair(
    named = function() run_predictions(FALSE),
    plain = function() run_predictions(TRUE),
    reps = max(3L, ceiling(repetitions / 2))
)
cat("\nFull predictions() with delta-method inference\n")
print(full_predictions)

full_comparisons <- benchmark_pair(
    named = function() run_comparisons(FALSE),
    plain = function() run_comparisons(TRUE),
    reps = max(3L, ceiling(repetitions / 2))
)
cat("\nFull avg_comparisons() with delta-method inference\n")
print(full_comparisons)
