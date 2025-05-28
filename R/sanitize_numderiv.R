sanitize_numderiv <- function(numderiv) {
    checkmate::assert(
        checkmate::check_choice(numderiv, c("richardson", "fdforward", "fdcenter")),
        checkmate::check_list(numderiv, min.len = 1)
    )

    if (isTRUE(checkmate::check_string(numderiv))) {
        numderiv <- list(numderiv)
    }

    if (length(numderiv) > 1) {
        if (numderiv[[1L]] %in% c("fdforward", "fdcenter")) {
            if (!all(names(numderiv)[-1L] %in% "eps")) {
                stop(
                    "The only valid argument for this numeric differentiation method is `eps`.",
                    call. = FALSE
                )
            }
        } else if (numderiv[[1L]] == "richardson") {
            valid <- c("eps", "d", "zero_tol", "size", "r", "v")
            if (!all(names(numderiv)[-1L] %in% valid)) {
                stop(
                    sprintf(
                        "The only valid arguments for this numeric differentiation method are: %s. See `?numDeriv::grad` for details.",
                        toString(valid)
                    ),
                    call. = FALSE
                )
            }
        }
    }

    return(numderiv)
}
