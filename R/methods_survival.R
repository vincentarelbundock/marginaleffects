#' @rdname get_predict
#' @export
get_predict.coxph <- function(model,
                              newdata = insight::get_data(model),
                              type = "lp",
                              ...) {

    out <- stats::predict(model,
                          newdata = newdata,
                          type = type,
                          ...)

    out <- data.frame(rowid = seq_len(nrow(newdata)),
                      estimate = out)
    return(out)
}
