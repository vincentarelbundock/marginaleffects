#' @rdname get_predict
#' @export
get_predict.clm <- function(
    model,
    newdata = get_modeldata(model),
    type = "prob",
    ...) {
    # `predict.clm()` only makes predictions for the observed response group of
    # each observation in `newdata`. When we remove the response from
    # `newdata`, `predict.clm()` makes predictions for all levels, which is
    # what we want.
    resp <- insight::find_response(model)

    # otherwise `predict.clm` does not see some columns (mystery)
    # copy to avoid breakage in the comparison plan
    newdata <- as.data.frame(newdata)

    newdata <- newdata[, setdiff(colnames(newdata), resp), drop = FALSE]

    pred <- stats::predict(model, newdata = newdata, type = type)

    contenders <- c("fit", "eta1", "eta2", "cprob1", "cprob2")
    tmp <- NULL
    for (con in contenders) {
        if (is.null(tmp) && con %in% names(pred)) {
            tmp <- pred[[con]]
        }
    }
    pred <- tmp

    out <- data.table(
        group = rep(colnames(pred), each = nrow(pred)),
        estimate = c(pred)
    )
    out$group <- group_to_factor(out$group, model)
    out <- add_rowid(out, newdata)
    return(out)
}

#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.clm <- get_group_names.polr

#' @rdname get_predict
#' @export
get_predict.clmm2 <- function(
    model,
    newdata = get_modeldata(model),
    type = "prob",
    ...) {
    # `get_predict.clm()` gets one probability per response level by *deleting*
    # the response from `newdata`. That trick does not carry over: `predict.clm2()`
    # builds its model frame from the full formula, so a missing response is an
    # error rather than a request for every level. It also takes no `type`
    # argument and always returns the probability of the category recorded in
    # `newdata`. Loop over the levels, overwriting the response each time.
    newdata <- as.data.frame(newdata)
    resp <- insight::find_response(model)
    lev <- model[["lev"]]

    # The training response fixes the class that `.checkMFClasses()` demands;
    # an ordered factor must stay ordered.
    y <- model[["location"]][[resp]]
    ordered <- isTRUE(is.ordered(y))

    pred <- vapply(
        lev,
        function(k) {
            nd <- newdata
            nd[[resp]] <- factor(k, levels = lev, ordered = ordered)
            stats::predict(model, newdata = nd)
        },
        numeric(nrow(newdata))
    )
    pred <- matrix(pred, nrow = nrow(newdata), dimnames = list(NULL, lev))

    out <- data.table(
        group = rep(colnames(pred), each = nrow(pred)),
        estimate = c(pred)
    )
    out$group <- group_to_factor(out$group, model)
    out <- add_rowid(out, newdata)
    return(out)
}


#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.clmm2 <- get_group_names.polr



#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
#' @export
sanitize_model_specific.clm <- function(model, ...) {
    # Corner case: The `predict.clm` method does not make predictions when the
    # response was transformed to a factor in the formula AND the response is
    # missing from `newdata`.
    lhs <- names(attr(stats::terms(model), "dataClasses"))[1]
    if (isTRUE(grepl("^factor\\(", lhs))) {
        stop(
            "The response variable should not be transformed to a factor in the formula. Please convert the variable to factor before fitting your model.",
            call. = FALSE
        )
    }
    return(model)
}


#' Threshold Jacobian mapping `Alpha` (the estimated, possibly reduced,
#' threshold parametrization reported by `vcov()`) to `Theta` (the full vector
#' of cut-points that `predict.clm2()` consumes).
#'
#' @keywords internal
#' @noRd
get_tJac_clmm2 <- function(model) {
    # `makeThresholds()` returns the identity for `threshold = "flexible"`, so
    # the flexible case needs no special handling. `threshold` is a `match.arg()`
    # formal of `clmm2()` and is always populated.
    fun <- get("makeThresholds", asNamespace("ordinal"))
    fun(model$lev, model$threshold)$tJac
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
#' @export
sanitize_model_specific.clmm2 <- function(model, ...) {
    # `zeta` (scale) and `nominal` effects duplicate the coefficient names that
    # `vcov()` reports, which makes it impossible to align the variance matrix
    # with the coefficient vector by name. Rather than return silently wrong
    # standard errors, we reject those models.
    if (length(model[["zeta"]]) > 0) {
        stop(
            "`clmm2` models with a `scale` component are not supported by `marginaleffects`.",
            call. = FALSE
        )
    }
    if (!is.null(model[["nominal"]])) {
        stop(
            "`clmm2` models with a `nominal` component are not supported by `marginaleffects`.",
            call. = FALSE
        )
    }
    # `insight::get_data()` cannot recover the training data for a `clmm2`
    # fitted anywhere but the global environment: it silently falls back to
    # reconstructing a frame from the model object, which loses factor types
    # and the rows added by `weights`. `model$location` is the model frame
    # `clmm2()` stored at fit time, so attach it directly.
    if (is.null(attr(model, "marginaleffects_modeldata"))) {
        mf <- model[["location"]]
        if (is.data.frame(mf) && nrow(mf) > 0) {
            mf <- mf[, !grepl("^\\(.*\\)$", colnames(mf)), drop = FALSE]
            model <- set_modeldata(model, mf)
        }
    }
    return(model)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.clmm2 <- function(model, coefs, ...) {
    # `predict.clm2()` (which also serves `clmm2`) builds its linear predictor
    # from `Theta` and `beta`. `Alpha` and its alias `xi` hold the estimated,
    # possibly reduced, threshold parametrization that `vcov()` reports.
    # Writing to `Alpha` alone leaves `Theta` frozen, which zeroes out the
    # threshold columns of the Jacobian and yields missing standard errors.
    # The two coincide only when `threshold = "flexible"`.
    idx <- 0L
    if (length(model[["Alpha"]]) > 0) {
        idx_alpha <- seq_along(model$Alpha)
        model$Alpha[] <- coefs[idx_alpha]
        if (length(model[["xi"]]) > 0) {
            model$xi[] <- coefs[idx_alpha]
        }
        model$Theta[] <- drop(get_tJac_clmm2(model) %*% model$Alpha)
        idx <- length(model$Alpha)
    }
    if (length(model[["beta"]]) > 0) {
        model$beta[] <- coefs[seq_along(model$beta) + idx]
        idx <- idx + length(model$beta)
    }
    # Aranda-Ordaz and log-gamma links estimate a link parameter which
    # `predict.clm2()` uses and `vcov()` reports.
    if (isTRUE(model[["estimLambda"]] > 0)) {
        model$lambda[] <- coefs[idx + 1L]
        idx <- idx + 1L
    }
    # `coefficients` is not used for prediction, but keep it consistent for
    # `print()` and `summary()`. Its tail holds the random-effect standard
    # deviation, which is not a coefficient we perturb.
    model$coefficients[seq_len(idx)] <- coefs[seq_len(idx)]
    return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.clmm2 <- function(model, ...) {
    out <- c(model$Alpha, model$beta)
    if (isTRUE(model[["estimLambda"]] > 0)) {
        out <- c(out, model$lambda)
    }
    return(out)
}


#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.clmm2 <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !is.logical(vcov)) {
        stop_sprintf(
            "The `vcov` for this class of models must be TRUE or FALSE."
        )
    }
    vcov <- sanitize_vcov(model, vcov)
    if (isFALSE(vcov)) {
        return(NULL)
    }
    # `insight::get_varcov()` drops the link parameter estimated by the
    # Aranda-Ordaz and log-gamma links, which `get_coef()` reports. Take the
    # matrix from `ordinal` directly and align it on the coefficient names.
    # This also drops the trailing random-effect standard deviation, which is
    # not one of the parameters we perturb.
    out <- try(stats::vcov(model), silent = TRUE)
    if (inherits(out, "try-error")) {
        return(NULL)
    }
    nms <- names(get_coef(model))
    if (!all(nms %in% colnames(out))) {
        return(NULL)
    }
    out <- out[nms, nms, drop = FALSE]
    return(out)
}
