#' find mode, preserve type, and pick an arbitrary value when multi-modal
#' https://stackoverflow.com/a/8189441/342331
#' @noRd
get_mode <- function(x) {
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
#' @keywords internal
#' @noRd
#' @return numeric vector
get_mean_or_mode <- function(x) {
    UseMethod("get_mean_or_mode")
}

#' @export
get_mean_or_mode.default <- function(x) {
    mean(x)
}

#' @export
get_mean_or_mode.character <- function(x) {
    get_mode(x)
}

#' @export
get_mean_or_mode.factor <- function(x) {
    get_mode(x)
}

#' @export
get_mean_or_mode.logical <- function(x) {
    get_mode(x)
}

#' @export
get_mean_or_mode.data.frame <- function(x) {
    out <- list()
    for (n in names(x)) {
        # variables transformed to factor in formula are assigned a "factor"
        # TRUE attribute by insight::get_data
        if (isTRUE(attributes(x)$marginaleffects_variable_class[[n]] == "factor")) {
            out[[n]] <- get_mean_or_mode.factor(x[[n]])
        } else {
            out[[n]] <- get_mean_or_mode(x[[n]])
        }
    }
    return(out)
}
