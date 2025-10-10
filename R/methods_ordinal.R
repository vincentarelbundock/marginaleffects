#' @rdname get_predict
#' @export
get_predict.clm <- function(
    model,
    newdata = insight::get_data(model),
    type = "prob",
    ...) {
    # `predict.clm()` only makes predictions for the observed response group of
    # each observation in `newdata`. When we remove the response from
    # `newdata`, `predict.clm()` makes predictions for all levels, which is
    # what we want.
    resp <- insight::find_response(model)

    # otherwise `predict.clm` does not see some columns (mystery)
    # copy to avoid breakage in get_comparisons()
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


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
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


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.clmm2 <- function(model, coefs, ...) {
    # clmm2 models store coefficients in multiple places:
    # - model$beta: fixed effects only (named vector)
    # - model$Alpha (also Theta, xi): threshold parameters (named vector)
    # - model$coefficients: all parameters including thresholds, fixed effects, and random SD

    # Update fixed effects (beta)
    if (!is.null(model$beta)) {
        idx <- match(names(model$beta), names(coefs))
        idx <- idx[!is.na(idx)]
        if (length(idx) > 0) {
            model$beta[names(coefs[idx])] <- coefs[idx]
        }
    }

    # Update threshold parameters (Alpha, Theta, xi)
    if (!is.null(model$Alpha)) {
        idx <- match(names(model$Alpha), names(coefs))
        idx <- idx[!is.na(idx)]
        if (length(idx) > 0) {
            model$Alpha[names(coefs[idx])] <- coefs[idx]
            model$xi <- model$Alpha

            # Compute Theta from Alpha using the threshold transformation
            # For non-flexible thresholds, Theta = Alpha %*% t(tJac)
            if (!is.null(model$threshold) && model$threshold != "flexible") {
                thresh_info <- ordinal:::makeThresholds(model$lev, model$threshold)
                model$Theta <- c(model$Alpha %*% t(thresh_info$tJac))
                names(model$Theta) <- paste(model$lev[-length(model$lev)],
                                           model$lev[-1],
                                           sep = "|")
            } else {
                # For flexible thresholds, Theta = Alpha
                model$Theta <- model$Alpha
            }
        }
    }

    # Update combined coefficients vector
    # Note: model$coefficients includes random effect SD (last element with empty name)
    # We only update the named coefficients that match our coefs
    if (!is.null(model$coefficients)) {
        coef_names <- names(model$coefficients)
        # Exclude empty names (random effect SD)
        named_idx <- which(coef_names != "" & !is.na(coef_names))
        for (i in named_idx) {
            if (coef_names[i] %in% names(coefs)) {
                model$coefficients[i] <- coefs[coef_names[i]]
            }
        }
    }

    return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.clmm2 <- function(model, ...) {
    # Use insight::get_parameters which excludes random effect parameters
    out <- insight::get_parameters(model, component = "conditional")
    out <- stats::setNames(out$Estimate, out$Parameter)
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.clmm2 <- function(
    model,
    newdata = insight::get_data(model),
    type = "prob",
    ...) {

    # Unlike clm, clmm2 predict method requires the response variable to be present
    # in newdata, even though the predictions don't depend on its value
    newdata <- as.data.frame(newdata)

    # clmm2 predict returns a single probability vector
    pred <- stats::predict(model, newdata = newdata, ...)

    out <- data.table::data.table(estimate = pred)
    out <- add_rowid(out, newdata)

    return(out)
}
