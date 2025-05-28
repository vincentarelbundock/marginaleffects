#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.data.frame <- function(model, ...) {
    checkmate::assert_data_frame(model)
    if (!"estimate" %in% colnames(model)) {
        insight::format_error(
            "The model object is a data.frame but doesn't contain the column 'estimate'. Make sure these columns are present"
        )
    }

    out <- model$estimate
    if ("term" %in% colnames(model)) {
        names(out) <- model$term
    } else {
        names(out) <- seq_along(out)
    }

    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.data.frame <- function(model, coefs, ...) {
    model$estimate = coefs
    return(model)
}
