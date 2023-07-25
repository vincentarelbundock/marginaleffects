#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.data.frame <- function(model, ...) {
    checkmate::assert_data_frame(model)
    if (!all(c("term", "estimate") %in% colnames(model))) {
        insight::format_error("The model object is a data.frame but doesn't contain the columns 'term' or 'estimate'. Make sure these columns are present")
    }

    out = model$estimate
    names(out) = model$term
    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.data.frame = function(model, coefs, ...) {
  model$estimate = coefs
  return(model)
}
