#' @title Takes a model object and returns a named *vector* of coefficients
#' @rdname get_coef
#' @export
get_coef <- function (model, ...) {
    UseMethod("get_coef", model)
}

get_coef.default <- function(model, ...) {
    stats::coef(model)
}
