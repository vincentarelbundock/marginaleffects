inferences_fwb <- function(x, R = 1000, conf_level = 0.95, conf_type = "perc", mfx = NULL, ...) {
    insight::check_if_installed("fwb", minimum_version = "0.5.0")

    out <- x
    call_mfx <- mfx@call
    call_mfx[["vcov"]] <- FALSE

    # Get modeldata from mfx object
    modeldata <- mfx@modeldata

    # Ensure parameters are embedded in the call, not just references
    if (!is.null(mfx@newdata)) {
        call_mfx[["newdata"]] <- mfx@newdata
    }
    if (!is.null(mfx@comparison)) {
        call_mfx[["comparison"]] <- mfx@comparison
    }

    # The random weights reweight the averaging over the rows of `newdata`
    # which are rows of the model data: the model data itself, in any order, a
    # subset, or a counterfactual grid. `idx` maps each row of `newdata` to its
    # row in the model data. Otherwise, the model is refit with the random
    # weights, but the averaging over `newdata` is fixed.
    wts_user <- mfx@wts
    idx <- NULL
    if (fwb_is_averaged(mfx)) {
        idx <- fwb_row_index(mfx)
        if (is.null(idx)) {
            msg <- "`inferences(method = \"fwb\")`: the rows of `newdata` are not rows of the data used to fit the model. The bootstrap weights are used to refit the model, but not to reweight the averaging over `newdata`, which is treated as fixed. To average over a subset of the model data, use `newdata = subset(...)`."
            warn_once(msg, "marginaleffects_fwb_newdata_fixed")
        }
    }
    # The call may refer to `wts` by name; use the resolved numeric weights.
    if (is.null(idx) && !is.null(wts_user)) {
        call_mfx[["wts"]] <- wts_user
    }

    bootfun <- function(data, w, ...) {
        # The averaging is reweighted by the random weights only: the model
        # weights enter the averaging through `wts`, as in the point estimate.
        w_fwb <- w
        # If model has weights, multiply them by random weights
        if (!is.null(w0 <- stats::weights(mfx@model))) {
            w <- w * w0
        }

        # Update the model's call and evaluate on `modeldata` (the rows the
        # weights correspond to; NA rows already dropped), in the formula
        # environment so function-local data and variables resolve.
        call_mod <- insight::get_call(mfx@model)
        call_mod[["weights"]] <- w
        if ("data" %in% names(call_mod)) {
            call_mod[["data"]] <- data
        }
        env <- tryCatch(environment(stats::formula(mfx@model)), error = function(e) NULL)
        if (is.null(env)) {
            env <- parent.frame()
        }
        boot_mod <- eval(call_mod, envir = env)

        # Update marginaleffects call
        call_mfx[["model"]] <- boot_mod
        call_mfx[["modeldata"]] <- data
        if (!is.null(idx)) {
            w_avg <- w_fwb[idx]
            call_mfx[["wts"]] <- if (is.null(wts_user)) w_avg else w_avg * wts_user
        }
        boot_mfx <- eval.parent(call_mfx)
        return(boot_mfx$estimate)
    }

    args <- list("data" = modeldata, "statistic" = bootfun, R = R)
    args <- c(args, list(...))

    # fwb default verbose is TRUE
    if (!"verbose" %in% names(args)) {
        args[["verbose"]] <- FALSE
    }

    if (
        isTRUE(getOption("marginaleffects_parallel_inferences", default = FALSE)) &&
            !"cl" %in% names(args)
    ) {
        args[["cl"]] <- "future"
    }

    B <- do.call(fwb::fwb, args)

    # Extract SEs and CIs
    fwb_summary <- tidy(summary(B, conf = conf_level, ci.type = conf_type, p.value = TRUE))

    out$std.error <- fwb_summary$std.error
    out$conf.low <- fwb_summary$conf.low
    out$conf.high <- fwb_summary$conf.high

    cols <- setdiff(names(out), "df")

    if ("p.value" %in% names(fwb_summary)) {
        out$p.value <- fwb_summary$p.value
        out$s.value <- -log2(out$p.value)
    } else {
        cols <- setdiff(cols, c("s.value", "p.value"))
    }

    if ("statistic" %in% names(fwb_summary)) {
        out$statistic <- fwb_summary$statistic
    } else {
        cols <- setdiff(cols, "statistic")
    }

    out <- out[, cols, drop = FALSE]

    mfx@draws <- t(B$t)
    mfx@inferences <- B
    attr(out, "marginaleffects") <- mfx

    return(out)
}


# Weights only matter when unit-level estimates are averaged.
fwb_is_averaged <- function(mfx) {
    if (!inherits(mfx@newdata, "data.frame") || nrow(mfx@newdata) == 0) {
        return(FALSE)
    }
    comparison <- mfx@comparison
    !isFALSE(mfx@by) ||
        is.function(comparison) ||
        (is.character(comparison) && any(grepl("avg", comparison)))
}


# Map each row of `newdata` to its row in the model data, or return NULL when
# some rows of `newdata` are not rows of the model data.
#
# 1. Row identifiers, where available: `rowidcf` in a counterfactual grid, and
#    `rowid` when `newdata` is the model data. An identifier is only trusted
#    after checking the values of the rows it points to, because `rowid` is
#    `seq_len(nrow(newdata))` for any other `newdata`. This check is cheap.
# 2. Otherwise, match rows on their values. This is slower.
#
# Values are compared exactly: a row shifted by a small step is another row.
fwb_row_index <- function(mfx) {
    newdata <- mfx@newdata
    modeldata <- mfx@modeldata
    if (!inherits(newdata, "data.frame") || !inherits(modeldata, "data.frame")) {
        return(NULL)
    }
    n <- nrow(modeldata)
    ignored <- c("rowid", "rowidcf", "rowid_dedup", "marginaleffects_wts_internal")
    common <- setdiff(intersect(colnames(newdata), colnames(modeldata)), ignored)
    # a counterfactual grid overwrites these variables in copies of each row
    overwritten <- c(mfx@variable_names_datagrid, names(mfx@variables))

    # `as.vector()` drops classes and dimensions: factors become character,
    # and a `poly()` matrix compares to its plain matrix subset.
    same_values <- function(a, b) {
        isTRUE(all.equal(as.vector(a), as.vector(b), check.attributes = FALSE, tolerance = 0))
    }
    take <- function(x, i) if (length(dim(x)) == 2) x[i, , drop = FALSE] else x[i]

    for (id in intersect(c("rowidcf", "rowid"), colnames(newdata))) {
        candidate <- newdata[[id]]
        cols <- if (id == "rowidcf") setdiff(common, overwritten) else common
        valid <- is.numeric(candidate) &&
            length(cols) > 0 &&
            !anyNA(candidate) &&
            all(candidate >= 1 & candidate <= n & candidate == round(candidate))
        if (!valid) {
            next
        }
        same <- TRUE
        for (v in cols) {
            if (!same_values(newdata[[v]], take(modeldata[[v]], candidate))) {
                same <- FALSE
                break
            }
        }
        if (same) {
            return(as.integer(candidate))
        }
    }

    fwb_match_rows(newdata, modeldata, common)
}


# Match rows on their values. Among identical rows of the model data, the k-th
# copy in `newdata` is paired with the k-th copy in the model data: identical
# rows are interchangeable, but each one keeps its own weight.
fwb_match_rows <- function(newdata, modeldata, common) {
    if (length(common) == 0) {
        return(NULL)
    }
    keys <- sprintf("marginaleffects_key_%s", seq_along(common))
    key_table <- function(dat) {
        out <- lapply(common, function(v) {
            x <- dat[[v]]
            if (is.factor(x)) as.character(x) else x
        })
        if (!all(vapply(out, function(x) is.atomic(x) && is.null(dim(x)), logical(1)))) {
            stop("unsupported column")
        }
        out <- data.table::as.data.table(stats::setNames(out, keys))
        out[, "marginaleffects_copy" := seq_len(.N), by = keys]
        out
    }
    out <- tryCatch(
        {
            tab_model <- key_table(modeldata)
            tab_model[, "marginaleffects_row" := seq_len(.N)]
            tab_new <- key_table(newdata)
            on <- c(keys, "marginaleffects_copy")
            tab_model[tab_new, on = on][["marginaleffects_row"]]
        },
        error = function(e) NULL
    )
    if (is.null(out) || length(out) != nrow(newdata) || anyNA(out)) {
        return(NULL)
    }
    as.integer(out)
}
