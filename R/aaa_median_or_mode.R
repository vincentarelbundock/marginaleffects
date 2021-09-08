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
