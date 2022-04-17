#' @rdname get_predict
#' @export
get_predict.coxph <- function(model,
                              newdata = insight::get_data(model),
                              vcov = NULL,
                              type = "lp",
                              conf.level = NULL,
                              ...) {

    if (!isTRUE(checkmate::check_flag(vcov, null.ok = TRUE))) stop("The `vcov` argument is not supported for this model class.")

    out <- stats::predict(model,
                          newdata = newdata,
                          type = type,
                          ...)

    out <- data.frame(rowid = 1:nrow(newdata),
                      predicted = out)
    return(out)
}
