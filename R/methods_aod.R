#' @rdname get_predict
#' @export
get_predict.glimML <- function(model,
                               newdata = insight::get_data(model),
                               vcov = FALSE,
                               conf_level = 0.95,
                               type = "response",
                               ...) {

    assert_dependency("aod") # need access to the predict method
    out <- aod::predict(model,
                        newdata = newdata,
                        type = type,
                        ...)
    out <- data.frame(
        rowid = 1:nrow(newdata),
        predicted = out)

    return(out)
}


#' @rdname set_coef
#' @export
set_coef.glimML <- function(model, coefs) {
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
    aod::vcov(model)
}
