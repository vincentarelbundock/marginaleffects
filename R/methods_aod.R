#' @rdname get_predict
#' @export
get_predict.glimML <- function(model,
                               newdata = insight::get_data(model),
                               type = "response",
                               ...) {

    insight::check_if_installed("aod")

    out <- aod::predict(model,
                        newdata = newdata,
                        type = type,
                        ...)
    out <- data.frame(
        rowid = seq_len(nrow(newdata)),
        estimate = out)

    return(out)
}


#' @rdname set_coef
#' @export
set_coef.glimML <- function(model, coefs, ...) {
    # in basic model classes coefficients are named vector
    model@fixed.param[names(coefs)] <- coefs
    model
}


#' @rdname get_vcov
#' @export
get_vcov.glimML <- function(model, vcov = NULL, ...) {
    insight::check_if_installed("aod")
    if (!is.null(vcov) && !is.logical(vcov)) {
        stop("The `vcov` argument is not supported for this kind of model.")
    }
    vcov <- sanitize_vcov(model, vcov)
    aod::vcov(model)
}


#' @rdname sanitize_model_specific
sanitize_model_specific.glimML <- function(model, ...) {
    mdat <- get_modeldata(model, additional_variables = FALSE)
    if (isTRUE("character" %in% attr(mdat, "marginaleffects_variable_class"))) {
        insight::format_error("This function does not support character predictors. Please convert them to factors before fitting the model.")
    }
    return(model)
}
