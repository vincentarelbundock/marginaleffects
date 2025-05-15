sanitize_cross <- function(cross, variables, model) {
    # cross: flip NULL to TRUE if there are interactions in the formula
    # and FALSE otherwise

    checkmate::assert_flag(cross, null.ok = FALSE)

    if (isTRUE(cross) && is.null(variables)) {
        msg <- "When `cross = TRUE`, you must use the `variables` argument to specify which variables should be interacted."
        insight::format_error(msg)
    }

    if (isTRUE(checkmate::check_flag(cross))) {
        return(cross)
    }

    inter <- try(insight::find_interactions(model, flatten = TRUE), silent = TRUE)

    # variables is length 1 means we don't want an interaction
    if (length(variables) > 1 && isTRUE(length(inter) > 0)) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}
