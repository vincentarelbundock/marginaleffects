#' @rdname get_vcov
#' @export
get_vcov.orm <- function(model, vcov = NULL, ...) {
    if (!is.null(vcov) && !isTRUE(checkmate::check_flag(vcov))) {
        msg <- "The `vcov` argument is not supported for models of this class."
        stop_sprintf(msg)
    }
    vcov <- sanitize_vcov(model, vcov)
    out <- stats::vcov(model, intercepts = "all")
    return(out)
}


#' @rdname sanitize_model_specific
sanitize_model_specific.rms <- function(model, ...) {
    # Issue #1600
    md <- get_modeldata(model)
    ord <- sapply(md, function(x) is.factor(x) & inherits(x, "ordered"))
    if (any(ord)) {
        msg <- "Ordered factors sometimes cause issues with `rms` models. Please convert them to unordered factors and refit the model."
        warning(msg, call. = FALSE)
    }
    return(model)
}

#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.orm <- sanitize_model_specific.rms


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.lrm <- sanitize_model_specific.rms


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.ols <- sanitize_model_specific.rms


#' @rdname get_predict
#' @export
get_predict.rms <- function(
    model,
    newdata = get_modeldata(model),
    type = NULL,
    mfx = NULL,
    ...) {
    if (is.null(type)) {
        calling_function <- if (!is.null(mfx)) mfx@calling_function else "predictions"
        type <- sanitize_type(model, type, calling_function = calling_function)
    }
    if (inherits(newdata, "tbl_df")) {
        warning("Converting `newdata` from tibble to data.frame.", call. = FALSE)
        newdata <- as.data.frame(newdata)
    }

    # {rms} predict methods break on additional arguments
    out <- get_predict.default(model, newdata = newdata, type = type)
    out <- add_rowid(out, newdata)
    return(out)
}

#' @rdname get_predict
#' @export
get_predict.orm <- get_predict.rms


#' @rdname get_predict
#' @export
get_predict.lrm <- get_predict.rms


#' @rdname get_predict
#' @export
get_predict.ols <- get_predict.rms


get_model_matrix_rms <- function(model, newdata) {
    # predictrms(type = "x") is the authoritative RMS design construction,
    # including restricted splines and Design metadata. It omits intercepts.
    X <- stats::predict(model, newdata = newdata, type = "x")
    X <- as.matrix(X)
    beta <- get_coef(model)
    n_intercepts <- model$non.slopes
    if (
        length(n_intercepts) != 1L || !is.numeric(n_intercepts) ||
            n_intercepts < 1L ||
            ncol(X) != length(beta) - n_intercepts
    ) {
        return(NULL)
    }

    intercept_ref <- model$interceptRef
    if (length(intercept_ref) == 0L) {
        intercept_ref <- 1L
    }
    if (
        length(intercept_ref) != 1L || intercept_ref < 1L ||
            intercept_ref > n_intercepts
    ) {
        return(NULL)
    }

    intercepts <- matrix(0, nrow = nrow(X), ncol = n_intercepts)
    intercepts[, intercept_ref] <- 1
    out <- cbind(intercepts, X)
    colnames(out) <- names(beta)
    out
}


#' @rdname get_model_matrix
#' @export
get_model_matrix.ols <- function(model, newdata, mfx = NULL) {
    get_model_matrix_rms(model, newdata)
}


#' @rdname get_model_matrix
#' @export
get_model_matrix.lrm <- get_model_matrix.ols


#' @noRd
#' @export
get_prediction_jacobian_spec.ols <- function(model, type, ...) {
    prediction_jacobian_spec_linear(model, "ols", type, "lp")
}


#' @noRd
#' @export
get_prediction_jacobian_spec.lrm <- function(model, type, ...) {
    # The fitted scale is a plain logit only for a binary outcome with no
    # special link functions; ordinal or custom-family fits stay numeric.
    if (
        identical(type, "fitted") &&
            (!identical(model$non.slopes, 1L) || length(model$famfunctions) > 0L)
    ) {
        return(NULL)
    }
    prediction_jacobian_spec_glm_family(
        model,
        "lrm",
        type,
        response_type = "fitted",
        link_type = "lp",
        family = stats::binomial("logit")
    )
}
