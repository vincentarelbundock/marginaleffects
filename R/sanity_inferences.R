sanity_inferences_conformal <- function(mfx, score, method, data_calib, R) {
    checkmate::assert_choice(
        score,
        choices = c("residual_abs", "residual_sq", "softmax")
    )

    if (is.null(mfx) || !inherits(mfx, "marginaleffects_internal")) {
        stop_sprintf("Conformal prediction requires an object produced by `predictions()`.")
    }

    if (!identical(mfx@calling_function, "predictions")) {
        stop_sprintf("Conformal prediction currently supports outputs from `predictions()` only.")
    }

    if (!isFALSE(mfx@by)) {
        stop_sprintf("Conformal prediction requires `by = FALSE` in the original `predictions()` call.")
    }

    call <- mfx@call
    if (is.null(call)) {
        stop_sprintf("Unable to inspect the original `predictions()` call for conformal inference.")
    }

    call_args <- as.list(call)[-1]
    call_names <- names(call_args)

    # Detect ellipsis/unnamed arguments
    if (any(is.na(call_names) | call_names == "")) {
        stop_sprintf("Conformal prediction does not support additional unnamed arguments in `predictions()`.")
    }

    formals_pred <- as.list(formals(marginaleffects::predictions))
    formal_names <- names(formals_pred)

    # Reject arguments passed through ... (not formal arguments)
    extra_args <- setdiff(call_names, formal_names)
    if (length(extra_args) > 0) {
        stop_sprintf(
            "Conformal prediction only supports `predictions()` with `newdata`. Remove the following arguments: %s",
            paste(extra_args, collapse = ", ")
        )
    }

    # Only select arguments may differ from defaults
    allowed_args <- c("model", "newdata", "conf_level", "vcov", "...")
    restricted <- setdiff(formal_names, allowed_args)

    for (arg in restricted) {
        current <- if (arg %in% call_names) call_args[[arg]] else formals_pred[[arg]]
        default <- formals_pred[[arg]]
        if (!identical(current, default)) {
            stop_sprintf(
                "Conformal prediction only supports raw `predictions()` calls; please remove the `%s` argument.",
                arg
            )
        }
    }

    if (method %in% c("conformal_split", "conformal_quantile")) {
        checkmate::assert_data_frame(data_calib, null.ok = FALSE)
    }

    if (method == "conformal_cv+") {
        checkmate::assert_integerish(R, upper = 25)
    }

    if (method == "conformal_full" && score == "softmax") {
        msg <- "The 'softmax' score is not supported for full conformal prediction. Use 'residual_abs' or 'residual_sq' for regression tasks."
        stop_sprintf(msg)
    }

    return(invisible(NULL))
}


sanitize_estimator <- function(x, estimator, method) {
    checkmate::assert_function(estimator)
    checkmate::assert_data_frame(x)
    cl <- c("data.frame", "data.table", "tbl_df", "tbl")
    if (!any(cl %in% class(x)[1])) {
        msg <- "The `x` argument must be a raw data frame when using the `estimator` argument."
        stop_sprintf(msg)
    }
    if (!isTRUE(checkmate::check_choice(method, c("rsample", "boot")))) {
        stop_sprintf("The `estimator` argument is only supported when `method` is \"rsample\" or \"boot\".")
    }
    x <- estimator(x)
    cl <- c("predictions", "comparisons", "slopes", "hypotheses")
    if (!any(cl %in% class(x))) {
        msg <- sprintf("The `estimator` function must return a `marginaleffects` object.")
        stop_sprintf(msg)
    }
    return(x)
}
