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
get_jacobian_analytic.ols <- function(model, type, ...) {
    if (
        !identical(class(model)[1], "ols") ||
            !identical(type, "lp")
    ) {
        return(NULL)
    }
    jacobian_analytic_model_matrix(
        model = model,
        type = type,
        response_scale = FALSE,
        ...
    )
}


#' @noRd
#' @export
get_jacobian_analytic.lrm <- function(model, type, ...) {
    if (
        !identical(class(model)[1], "lrm") ||
            !isTRUE(type %in% c("lp", "fitted"))
    ) {
        return(NULL)
    }

    response_scale <- identical(type, "fitted")
    if (
        response_scale &&
            (!identical(model$non.slopes, 1L) || length(model$famfunctions) > 0L)
    ) {
        return(NULL)
    }
    jacobian_analytic_model_matrix(
        model = model,
        type = type,
        response_scale = response_scale,
        family = if (response_scale) stats::binomial("logit") else NULL,
        ...
    )
}



### AUTODIFF: 
### The analytic path above uses predictrms(type = "x"). Keep the old JAX
### sketches below disabled unless they can use that same RMS-native matrix.

# #' @keywords internal
# #' @export
# get_autodiff_args.ols <- function(model, mfx) {
#     # no inheritance! Important to avoid breaking other models
#     if (!class(model)[1] == "ols") {
#         return(NULL)
#     }
#
#     if (!is.null(model$offset)) {
#         autodiff_warning("models with offsets")
#         return(NULL)
#     }
#
#     if (!is.null(model$penalty)) {
#         autodiff_warning("models with offsets")
#         return(NULL)
#     }
#
#     # Check type support
#     if (!mfx@type %in% c("lp")) {
#         autodiff_warning(sprintf("`type='%s'`", mfx@type))
#         return(NULL)
#     }
#
#     # If all checks pass, return supported arguments
#     out <- list(model_type = "linear")
#     return(out)
# }
#
#
# #' @keywords internal
# #' @export
# get_autodiff_args.lrm <- function(model, mfx) {
#     # no inheritance! Important to avoid breaking other models
#     if (!class(model)[1] == "lrm") {
#         return(NULL)
#     }
#
#     if (!is.null(model$offset)) {
#         autodiff_warning("models with offsets")
#         return(NULL)
#     }
#
#     if (!is.null(model$penalty)) {
#         autodiff_warning("models with offsets")
#         return(NULL)
#     }
#
#     # Check type support
#     if (!mfx@type %in% c("fitted", "lp")) {
#         autodiff_warning(sprintf("`type='%s'`", mfx@type))
#         return(NULL)
#     }
#
#     # If all checks pass, return supported arguments
#     mAD <- settings_get("mAD")
#     out <- list(
#         model_type = "glm",
#         family_type = mAD$glm$families$Family$BINOMIAL,
#         link_type = mAD$glm$families$Link$LOGIT
#     )
#     return(out)
# }
