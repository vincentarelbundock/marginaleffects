#' Get a named variance-covariance matrix from a model object (internal function)
#'
#' @inheritParams marginaleffects
#' @return A named square matrix of variance and covariances. The names must match the coefficient names.
#' @rdname get_vcov
#' @keywords internal
#' @export
get_vcov <- function(model, ...) {
    UseMethod("get_vcov", model)
}


#' @rdname get_vcov
#' @export
get_vcov.default <- function(model, ...) {
    # suppress "Re-fitting to get Hessian"
    out <- suppressMessages(stats::vcov(model))
    # survival::coxph with 1 regressor produces a vector
    if (!isTRUE(checkmate::check_matrix(out))) {
        out <- insight::get_varcov(model)
    }
    return(out)
}
