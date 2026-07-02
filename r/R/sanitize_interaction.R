sanitize_cross <- function(cross, variables, model) {
    checkmate::assert_flag(cross, null.ok = FALSE)

    if (isTRUE(cross) && is.null(variables)) {
        msg <- "When `cross = TRUE`, you must use the `variables` argument to specify which variables should be interacted."
        stop_sprintf(msg)
    }

    return(cross)
}
