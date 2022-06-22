#' @rdname set_coef
#' @export
set_coef.biglm <- function(model, coefs) {
    # in basic model classes coefficients are named vector
    model[["qr"]][["thetab"]] <- coefs
    model
}



#' @rdname get_predict
#' @export
get_predict.biglm <- function(model,
                              newdata = insight::get_data(model),
                              vcov = FALSE,
                              conf_level = 0.95,
                              type = "response",
                              ...) {

    type <- sanitize_type(model, type)
    type_base <- unname(type)
    out <- stats::predict(
        model,
        newdata = newdata,
        type = type)
    out <- as.vector(out)
    out <- data.frame(
        rowid = seq_along(out),
        predicted = out)
    return(out)
}
