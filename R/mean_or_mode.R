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

#' Compute the mode or mean of `x`
#' @param x extract the mean or the mode of vector or data.frame x depending on its type
#' @rdname mean_or_mode
#' @keywords internal
#' @export
#' @return numeric vector
mean_or_mode <- function(x) {
    UseMethod("mean_or_mode")
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.default <- function(x) {
    mean(x)
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.character <- function(x) {
    Mode(x)
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.factor <- function(x) {
    Mode(x)
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.logical <- function(x) {
    Mode(x)
}

#' @rdname mean_or_mode
#' @export
mean_or_mode.data.frame <- function(x) {
    stats::setNames(lapply(x, mean_or_mode), names(x))
}
