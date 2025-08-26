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
    ...) {
    newdata <- mfx@newdata

    # sometimes we want the perturbed coefficients model supplied by get_se_delta().
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed

    out <- get_predict_error(
        model,
        newdata = newdata,
        type = type,
        mfx = mfx,
        ...
    )

    # this needs to be a data.table
    data.table::setDT(out)

    if (
        !"rowid" %in% colnames(out) &&
            "rowid" %in% colnames(mfx@newdata) &&
            nrow(out) == nrow(mfx@newdata)
    ) {
        out$rowid <- mfx@newdata$rowid
    }

    # extract attributes before setDT
    draws <- attr(out, "posterior_draws")

    # TODO: find a cheaper way to do this, but it's tricky
    # variables can come from:
    # - by: characters, data.frame, TRUE, groups
    # - wts
    # - hypothesis multi-part formulae
    cols <- setdiff(colnames(newdata), colnames(out))
    out <- cbind(out, subset(newdata, select = cols))

    # unpad factors before `by` and `hypothesis`
    tmp <- unpad(out, draws)
    list2env(tmp, envir = environment())


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
