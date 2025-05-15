#' Get a named vector of coefficients from a model object
#'
#' Mostly for internal use, but can be useful because the output is consistent across model classes.
#' @inheritParams slopes
#' @return A named vector of coefficients. The names must match those of the variance matrix.
#' @rdname get_coef
#' @export
get_coef <- function(model, ...) {
    UseMethod("get_coef", model)
}

#' @rdname get_coef
#' @export
get_coef.default <- function(model, ...) {
    ## faster
    # out <-  stats::coef(model)

    # more general
    out <- insight::get_parameters(model, component = "all")

    out <- stats::setNames(out$Estimate, out$Parameter)
    return(out)
}


#' @export
get_coef.predictions <- function(model, ...) {
    stats::coef(model)
}

#' @export
get_coef.comparisons <- get_coef.predictions

#' @export
get_coef.slopes <- get_coef.predictions
