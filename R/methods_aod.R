#' @rdname get_predict
#' @export
get_predict.glimML <- function(model,
                               newdata = insight::get_data(model),
                               type = "response",
                               ...) {

    assert_dependency("aod")
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
get_vcov.glimML <- function(model, ...) {
    assert_dependency("aod")
    out <- aod::vcov(model)
    return(out)
}
