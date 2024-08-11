#' @include get_predict.R
#' @rdname get_predict
#' @keywords internal
#' @export
get_predict.glmmTMB <- function(model,
                                newdata = insight::get_data(model),
                                type = "response",
                                newparams = NULL,
                                ...) {

    if (inherits(vcov, "vcov.glmmTMB")) {
        vcov <- vcov[[1]]
    }

    # hack to avoid re-optimization
    # see https://github.com/vincentarelbundock/marginaleffects/issues/1064
    b_vec <- model$obj$env$parList()$b
    if (length(b_vec)>0) {
        model$modelInfo$map$b <- factor(rep(NA,length(b_vec)))
    }

    np <- model$fit$par
    if (!is.null(newparams)) {
        np[seq_along(newparams)] <- newparams
    }

    out <- get_predict.default(
        model = model,
        newdata = newdata,
        type = type,
        allow.new.levels = TRUE, # otherwise we get errors in marginal_means()
        newparams = np,
        ...)

    return(out)
}



#' @include get_vcov.R
#' @rdname get_vcov
#' @export
get_vcov.glmmTMB <- function(model, ...) {
    out <- stats::vcov(model, full = TRUE)
    return(out)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.glmmTMB <- function(model, ...) {
    glmmTMB::fixef(model)$cond
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glmmTMB <- function(model, coefs, ...) {
    # use predict(`newparams`) for this kind of model
     return(model)
}

#' @rdname sanitize_model_specific
sanitize_model_specific.glmmTMB <- function(model, vcov = TRUE, re.form = NULL, ...) {
    # re.form=NA
    if (!isTRUE(checkmate::check_flag(vcov))) {
        msg <- "For this model type, `vcov` must be `TRUE` or `FALSE`."
        insight::format_error(msg)
    }
    if (!isTRUE(is.na(re.form))) {
        msg <- "For this model type, `marginaleffects` only takes into account the uncertainty in fixed-effect parameters. You can use the `re.form=NA` argument to acknowledge this explicitly and silence this warning."
        insight::format_warning(msg)
    }
    return(model)
}
