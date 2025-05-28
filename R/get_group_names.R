#' Get levels of the outcome variable in grouped or multivariate models
#'
#' @inheritParams slopes
#' @return A character vector
#' @rdname get_group_names
#' @keywords internal
#' @export
get_group_names <- function(model, ...) {
    UseMethod("get_group_names", model)
}


#' @rdname get_group_names
#' @export
get_group_names.default <- function(model, ...) {
    return("main_marginaleffect")
}
