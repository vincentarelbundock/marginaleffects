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

#' find mode, preserve type, and pick an arbitrary value when multi-modal
#' https://stackoverflow.com/a/8189441/342331
#' @noRd
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

####################################################################
#  The functions below were copied from the `prediction` package. ##
#  Copyright: Thomas J. Leeper 2016-2018                          ##
#  MIT License                                                    ##
####################################################################

#' @rdname median_or_mode
#' @title Get typical values from data.frames or vectors
#' @param x extract the median or the mode of vector or data.frame x depending on its type
#' @export
median_or_mode <- function(x) {
    UseMethod("median_or_mode")
}

#' @rdname median_or_mode
#' @export
median_or_mode.default <- function(x) {
    stats::median(x)
}

#' @rdname median_or_mode
#' @export
median_or_mode.factor <- function(x) {
    Mode(x)
}

#' @rdname median_or_mode
#' @export
median_or_mode.logical <- function(x) {
    Mode(x)
}

#' @rdname median_or_mode
#' @export
median_or_mode.data.frame <- function(x) {
    stats::setNames(lapply(x, median_or_mode), names(x))
}
