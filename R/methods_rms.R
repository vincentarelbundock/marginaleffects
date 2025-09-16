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
    newdata = insight::get_data(model),
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



#' @keywords internal
#' @export
get_autodiff_args.ols <- function(model, mfx) {
    # no inheritance! Important to avoid breaking other models
    if (!class(model)[1] == "ols") {
        return(NULL)
    }

    if (!is.null(model$offset)) {
        autodiff_warning("models with offsets")
        return(NULL)
    }

    if (!is.null(model$penalty)) {
        autodiff_warning("models with offsets")
        return(NULL)
    }

    # Check type support
    if (!mfx@type %in% c("lp")) {
        autodiff_warning(sprintf("`type='%s'`", mfx@type))
        return(NULL)
    }

    # If all checks pass, return supported arguments
    out <- list(model_type = "linear")
    return(out)
}


#' @keywords internal
#' @export
get_autodiff_args.lrm <- function(model, mfx) {
    # no inheritance! Important to avoid breaking other models
    if (!class(model)[1] == "lrm") {
        return(NULL)
    }

    if (!is.null(model$offset)) {
        autodiff_warning("models with offsets")
        return(NULL)
    }

    if (!is.null(model$penalty)) {
        autodiff_warning("models with offsets")
        return(NULL)
    }

    # Check type support
    if (!mfx@type %in% c("fitted", "lp")) {
        autodiff_warning(sprintf("`type='%s'`", mfx@type))
        return(NULL)
    }

    # If all checks pass, return supported arguments
    mAD <- settings_get("mAD")
    out <- list(
        model_type = "glm",
        family_type = mAD$glm$families$Family$BINOMIAL,
        link_type = mAD$glm$families$Link$LOGIT
    )
    return(out)
}
