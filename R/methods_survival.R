#' @rdname get_predict
#' @export
get_predict.coxph <- function(model,
                              newdata = insight::get_data(model),
                              vcov = FALSE,
                              conf_level = 0.95,
                              type = "lp",
                              ...) {

    out <- stats::predict(model,
                          newdata = newdata,
                          type = type,
                          ...)

    out <- data.frame(rowid = seq_len(nrow(newdata)),
                      predicted = out)
    return(out)
}
