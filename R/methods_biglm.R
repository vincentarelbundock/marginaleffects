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
        estimate = out)
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.biglm <- function(model,
                           vcov = NULL,
                           ...) {

    if (!isFALSE(vcov)) {
        insight::format_warning(c("The `vcov` argument is not supported for this model type. Set `vcov=FALSE` to silence this warning, and visit this link to learn why standard errors for this model are not yet supported and how you can help:", "https://github.com/vincentarelbundock/marginaleffects/issues/387"))
    }

    return(FALSE)
}




