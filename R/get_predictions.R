get_predictions <- function(
    mfx,
    type,
    model_perturbed = NULL, # important for perturbed model
    by = NULL,
    byfun = byfun,
    hypothesis = NULL,
    verbose = TRUE,
    hi = NULL, # sink hole for shared comparisons/predictions call
    lo = NULL, # sink hole avoids pushing these variables through ... in get_predict()
    original = NULL, # sink hole
    ...
) {
    newdata <- mfx@newdata

    # sometimes we want the perturbed coefficients model supplied by get_se_delta().
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed

    out <- myTryCatch(get_predict(
        model,
        newdata = newdata,
        type = type,
        ...
    ))

    if (inherits(out$value, "data.frame")) {
        out <- out$value
    } else {
        # tidymodels
        if (
            inherits(out$error, "rlang_error") &&
                isTRUE(grepl("the object should be", out$error$message))
        ) {
            stop_sprintf(out$error$message)
        }

        msg <- "Unable to compute predicted values with this model. You can try to supply a different dataset to the `newdata` argument."
        if (!is.null(out$error)) {
            msg <- c(paste(msg, "This error was also raised:"), "", out$error$message)
        }
        if (inherits(out$value, "try-error")) {
            msg <- c(
                paste(msg, "", "This error was also raised:"),
                "",
                as.character(out$value)
            )
        }
        msg <- c(
            msg,
            "",
            "Bug Tracker: https://github.com/vincentarelbundock/marginaleffects/issues"
        )
        stop_sprintf(msg)
    }

    if (
        !"rowid" %in% colnames(out) &&
            "rowid" %in% colnames(mfx@newdata) &&
            nrow(out) == nrow(mfx@newdata)
    ) {
        out$rowid <- mfx@newdata$rowid
    }

    # extract attributes before setDT
    draws <- attr(out, "posterior_draws")

    data.table::setDT(out)

    # unpad factors before averaging
    # trust `newdata` rowid more than `out` because sometimes `get_predict()` will add a positive index even on padded data
    # HACK: the padding indexing rowid code is still a mess
    # Do not merge `newdata` with `hypothesis`, because it may have the same
    # number of rows but represent different quantities
    if (
        "rowid" %in%
            colnames(newdata) &&
            nrow(newdata) == nrow(out) &&
            is.null(hypothesis)
    ) {
        out$rowid <- newdata$rowid
    }
    # unpad
    if ("rowid" %in% colnames(out)) {
        draws <- draws[out$rowid > 0, , drop = FALSE]
    }
    if ("rowid" %in% colnames(out)) {
        out <- out[out$rowid > 0, , drop = FALSE]
    }
    if ("rowid" %in% colnames(newdata)) {
        newdata <- newdata[newdata$rowid > 0, , drop = FALSE]
    }

    # expensive: only do this inside the jacobian if necessary
    if (
        !is.null(mfx@wts) ||
            !isTRUE(checkmate::check_flag(by, null.ok = TRUE)) ||
            inherits(model, "mclogit")
    ) {
        # not sure why sorting is so finicky here
        out <- merge_by_rowid(out, newdata)
    }

    # by: auto group
    if (isTRUE(checkmate::check_character(by))) {
        by <- intersect(c("group", by), colnames(out))
    }

    # averaging by groups
    out <- get_by(
        out,
        draws = draws,
        newdata = newdata,
        by = by,
        byfun = byfun,
        verbose = verbose,
        ...
    )

    draws <- attr(out, "posterior_draws")

    # hypothesis tests using the delta method
    out <- get_hypothesis(
        out,
        hypothesis = hypothesis,
        by = by,
        newdata = newdata,
        draws = draws
    )

    return(out)
}
