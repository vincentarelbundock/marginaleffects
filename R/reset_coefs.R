#' @title Utility function to reset the coefficients in a model object
#' @rdname reset_coefs
#' @param model object to modify
#' @param coefs vector of coefficients to insert in the model object
#' @export
# To compute the variance of marginal effects we need to take the Jacobian with
# respect to the model coefficients. These functions manipulate model objects
# to change the coefficients stored internally, which changes the output of the
# `predict()` function.
reset_coefs <- function(model, coefs) {
    UseMethod("reset_coefs")
}

#' @rdname reset_coefs
#' @export
reset_coefs.default <- function(model, coefs) {
    # in basic model classes coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    model
}
