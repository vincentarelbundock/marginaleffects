#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.svy_vglm <- function(model, ...) {
    out <- model[["coef"]]
    if (is.null(names(out))) {
        fit_coefs <- try(model[["fit"]]@coefficients, silent = TRUE)
        if (!inherits(fit_coefs, "try-error") && length(fit_coefs) == length(out)) {
            names(out) <- names(fit_coefs)
        }
    }
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.svy_vglm <- function(model, coefs, ...) {
    model[["coef"]] <- sub_named_vector(model[["coef"]], coefs)

    fit_coefs <- try(model[["fit"]]@coefficients, silent = TRUE)
    if (!inherits(fit_coefs, "try-error") && isTRUE(checkmate::check_numeric(fit_coefs))) {
        model[["fit"]]@coefficients <- sub_named_vector(fit_coefs, coefs)
    }

    return(model)
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.svy_vglm <- function(
    model,
    newdata = get_modeldata(model),
    type = "response",
    mfx = NULL,
    newparams = NULL,
    ndraws = NULL,
    se.fit = NULL,
    ...) {
    calling_function <- if (!is.null(mfx)) mfx@calling_function else "predictions"
    type <- sanitize_type(model, type, calling_function = calling_function)

    pred <- stats::predict(model, newdata = newdata, type = type, ...)

    if (is.array(pred) && length(dim(pred)) == 1) {
        pred <- as.vector(pred)
    }

    if (
        isTRUE(checkmate::check_atomic_vector(pred)) &&
            length(pred) != nrow(newdata) &&
            !is.null(names(pred))
    ) {
        pred <- matrix(pred, nrow = 1, dimnames = list(NULL, names(pred)))
    }

    if (isTRUE(checkmate::check_atomic_vector(pred))) {
        out <- data.frame(estimate = as.vector(pred))
    } else if (is.matrix(pred) || is.data.frame(pred)) {
        pred <- as.matrix(pred)
        if (is.null(colnames(pred))) {
            colnames(pred) <- seq_len(ncol(pred))
        }
        out <- data.frame(
            group = rep(colnames(pred), each = nrow(pred)),
            estimate = c(pred)
        )
        out$group <- group_to_factor(out$group, model)
    } else {
        stop_sprintf(
            "Unable to extract predictions of type %s from a model of class %s.",
            type,
            class(model)[1]
        )
    }

    out <- add_rowid(out, newdata)
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.svy_vglm <- function(model, vcov = NULL, ...) {
    if (isFALSE(vcov)) {
        return(NULL)
    }
    if (isTRUE(checkmate::check_matrix(vcov))) {
        return(sanitize_vcov(model, vcov))
    }
    if (!is.null(vcov) && !is.logical(vcov)) {
        stop_sprintf(
            "The `vcov` argument for `svyVGAM::svy_vglm()` models must be TRUE, FALSE, or a matrix."
        )
    }

    out <- model[["var"]]
    if (!isTRUE(checkmate::check_matrix(out))) {
        return(NULL)
    }

    coefs <- get_coef(model)
    coef_names <- names(coefs)
    if (nrow(out) == length(coefs) && ncol(out) == length(coefs)) {
        if (is.null(rownames(out))) {
            rownames(out) <- coef_names
        }
        if (is.null(colnames(out))) {
            colnames(out) <- coef_names
        }
        if (!is.null(coef_names) && all(coef_names %in% rownames(out)) && all(coef_names %in% colnames(out))) {
            out <- out[coef_names, coef_names, drop = FALSE]
        }
    }

    return(out)
}


#' @include get_modeldata.R
#' @keywords internal
#' @noRd
get_modeldata_svy_vglm <- function(model) {
    out <- try(model[["design"]][["variables"]], silent = TRUE)
    if (inherits(out, "try-error") || !isTRUE(checkmate::check_data_frame(out))) {
        return(NULL)
    }

    out <- data.table::copy(out)
    data.table::setDF(out)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.svy_vglm <- function(
    model,
    calling_function = NULL,
    ...) {
    modeldata <- get_modeldata_svy_vglm(model)
    if (!is.null(modeldata)) {
        model <- set_modeldata(model, modeldata)
    }

    if (identical(calling_function, "hypotheses")) {
        return(model)
    }

    wts <- ...get("wts", FALSE)
    by <- ...get("by", FALSE)
    if (isFALSE(wts) && !isFALSE(by)) {
        warning(
            "With models of this class, it is normally good practice to specify weights using the `wts` argument. Otherwise, weights will be ignored in the computation of quantities of interest.",
            call. = FALSE
        )
    }

    return(model)
}
