#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.brmsfit <- function(model,
                                newdata = insight::get_data(model),
                                type = "response",
                                ...) {
    type <- match.arg(type, choices = c("response", "link", "prediction"))
    type <- ifelse(type == "response", "expectation", type)
    pred <- suppressWarnings(insight::get_predicted(model, data = newdata, predict = type, ...))
    pred <- as.data.frame(pred)
    pred <- insight::standardize_names(pred, style = "broom")
    out <- as.numeric(pred$predicted)
    draws <- as.matrix(pred[, grepl("^iter\\.", colnames(pred))])
    attr(out, "posterior_draws") <- draws
    return(out)
}
