prediction_prepare_newdata <- function(mfx, model) {
    if (!"rowid" %in% colnames(mfx@newdata)) {
        mfx@newdata[["rowid"]] <- seq_len(nrow(mfx@newdata))
    }

    unpadded_newdata <- mfx@newdata
    mfx@newdata <- pad(model, mfx@newdata)
    mfx@newdata <- add_model_matrix_attribute(mfx)

    list(mfx = mfx, unpadded_newdata = unpadded_newdata)
}


prediction_attach_newdata_and_unpad <- function(out, draws, newdata, mfx) {
    cols <- setdiff(colnames(newdata), colnames(out))
    if (inherits(mfx@model, "mlogit")) {
        out <- merge_by_rowid(out, newdata)
    } else {
        nd <- subset(newdata, select = cols)
        out <- cbind(out, nd)
    }

    keep <- NULL
    if ("rowid" %in% colnames(out)) {
        idx_keep <- out$rowid > 0
        if (!all(idx_keep)) {
            keep <- which(idx_keep)
        }
    }

    tmp <- unpad(out, draws)
    list(out = tmp$out, draws = tmp$draws, keep = keep)
}


prediction_normalize_by <- function(by, out) {
    if (isTRUE(checkmate::check_character(by))) {
        by <- intersect(c("group", by), colnames(out))
    }
    by
}


prediction_plan_build <- function(
    mfx,
    type,
    model_perturbed = NULL,
    by = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    hi = NULL,
    lo = NULL,
    original = NULL,
    ...) {
    dots <- list(...)
    newdata <- mfx@newdata
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed

    out <- get_predict_error(
        model,
        newdata = newdata,
        type = type,
        mfx = mfx,
        ...
    )
    data.table::setDT(out)

    if (
        !"rowid" %in% colnames(out) &&
            "rowid" %in% colnames(mfx@newdata) &&
            nrow(out) == nrow(mfx@newdata)
    ) {
        out$rowid <- mfx@newdata$rowid
    }

    draws <- attr(out, "posterior_draws")
    raw_estimate <- out[["estimate"]]

    prepared <- prediction_attach_newdata_and_unpad(
        out = out,
        draws = draws,
        newdata = newdata,
        mfx = mfx
    )
    out <- prepared$out
    draws <- prepared$draws
    keep <- prepared$keep

    by <- prediction_normalize_by(by, out)

    if (is.null(draws)) {
        prediction_plan_build_frequentist(
            out = out,
            raw_estimate = raw_estimate,
            keep = keep,
            newdata = newdata,
            type = type,
            mfx = mfx,
            dots = dots,
            by = by,
            hypothesis = hypothesis,
            verbose = verbose,
            ...
        )
    } else {
        prediction_plan_build_bayesian(
            out = out,
            draws = draws,
            newdata = newdata,
            by = by,
            hypothesis = hypothesis,
            verbose = verbose,
            mfx = mfx,
            ...
        )
    }
}
