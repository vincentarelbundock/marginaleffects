#' @rdname get_predict
#' @export
get_predict.coxph <- function(model,
                              newdata = insight::get_data(model),
                              vcov = FALSE,
                              conf.level = NULL,
                              type = "lp",
                              ...) {

    out <- stats::predict(model,
                          newdata = newdata,
                          type = type,
                          ...)

    out <- data.frame(rowid = 1:nrow(newdata),
                      predicted = out)
    return(out)
}
