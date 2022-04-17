#' @rdname get_predict
#' @export
get_predict.glimML <- function(model,
                               newdata = insight::get_data(model),
                               vcov = NULL,
                               type = "response",
                               ...) {

    if (!isTRUE(checkmate::check_flag(vcov, null.ok = TRUE))) stop("The `vcov` argument is not supported for this model class.")

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
