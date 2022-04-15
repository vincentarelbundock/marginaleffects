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
get_vcov.default <- function(model,
                             vcov = NULL,
                             ...) {

    # user-level default is TRUE, but this generates a warning in
    # `insight::get_varcov` for some models
    if (isTRUE(vcov)) {
        vcov <- NULL
    }
    out <- insight::get_varcov(model, vcov = vcov, component = "all")

    if (!isTRUE(checkmate::check_matrix(out))) {
        # suppress: "Re-fitting to get Hessian"
        out <- suppressMessages(stats::vcov(model, vcov = vcov))
    }

    if (is.null(row.names(out))) {
        termnames <- names(stats::coef(model))
        if (length(termnames) == ncol(out)) {
            colnames(out) <- termnames
            row.names(out) <- termnames
        }
    }

    return(out)


    # NOTES:
    # survival::coxph with 1 regressor produces a vector

}
