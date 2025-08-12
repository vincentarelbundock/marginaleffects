sanity_multcomp <- function(multcomp, hypothesis, joint) {
    # I don't know how to adjust p values for a different null in `multcomp::glht()`
    if (!isFALSE(multcomp) && isTRUE(checkmate::check_number(hypothesis))) {
        msg <- "The `multcomp` argument is not available when `hypothesis` is a number."
        stop(msg, call. = FALSE)
    }
    if (!isFALSE(multcomp) && !isFALSE(joint)) {
        msg <- "The `multcomp` argument cannot be used with the `joint` argument."
        stop_sprintf(msg)
    }
    return(invisible(NULL))
}
