sanitize_interaction <- function(interaction, variables, model) {
    # interaction: flip NULL to TRUE if there are interactions in the formula
    # and FALSE otherwise

    checkmate::assert_flag(interaction, null.ok = TRUE)

    if (isTRUE(interaction) && is.null(variables)) {
        msg <- format_msg(
        "When `interaction=TRUE` you must use the `variables` argument to specify which
        variables should be interacted.")
        stop(msg, call. = FALSE)
    }

    if (isTRUE(checkmate::check_flag(interaction))) {
        return(interaction)
    }

    inter <- try(insight::find_interactions(model, flatten = TRUE), silent = TRUE)

    # variables is length 1 means we don't want an interaction
    if (length(variables) > 1 && isTRUE(length(inter) > 0)) {
        return(TRUE)
    } else {
        return(FALSE)
    }

}


