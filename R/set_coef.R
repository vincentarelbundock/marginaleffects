#' Internal function to set coefficients
#'
#' Set the coefficients in a model to different values and return the modified object (internal function)
#'
#' @rdname set_coef
#' @param model object to modify
#' @param coefs vector of coefficients to insert in the model object
#' @export
#' @keywords internal
#' @return Model object of the same class as the `model` argument, but with
#'   different stored coefficients.
#' @details To compute the variance of marginal effects we need to take the
#' Jacobian with
# respect to the model coefficients. These functions manipulate model objects
# to change the coefficients stored internally, which changes the output of the
# `predict()` function.
set_coef <- function(model, coefs, ...) {
    UseMethod("set_coef")
}

#' @rdname set_coef
#' @export
set_coef.default <- function(model, coefs, ...) {
    # in basic model classes coefficients are named vector
    # in ordinal::clm models, there are sometimes duplicates, so name matching doesn't work
    if (all(names(model[["coefficients"]]) == names(coefs))) {
        model[["coefficients"]] <- coefs
    } else {
        model[["coefficients"]][names(coefs)] <- coefs
    }
    model
}
