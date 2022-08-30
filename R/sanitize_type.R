#' check type sanity
#'
#' @param model model object
#' @param type character vector
#' @return Named vector where value is the Base R `type` and name is the
#' insight::get_predicted `predict`
#' @noRd
sanitize_type <- function(model, type, calling_function = NULL) {

    checkmate::assert_character(type, len = 1, null.ok = TRUE)
    checkmate::assert_choice(calling_function,
                             choices = c("comparisons", "marginaleffects", "predictions", "marginalmeans"),
                             null.ok = TRUE)

    model_class <- class(model)[1]

    dict <- type_dictionary

    # default is the first type listed in type_dictionary
    if (is.null(type)) {
        idx <- match(model_class, dict$class)
        if (!is.na(idx)) {
            type <- dict$base[idx]
        } else {
            type <- "response"
        }
    }

    # optional subsetting
    if (!is.null(calling_function)) {
        dict <- dict[dict[[calling_function]] == TRUE, , drop = FALSE]
    }

    # known models are scrutinized tightly
    if (model_class %in% dict$class) {
        valid <- dict[dict$class == model_class, , drop = FALSE]
        if (!all(type %in% c(valid$base, valid$insight))) {
            msg <- sprintf("The `type` argument for models of class `%s` must be an element of: %s",
                           class(model)[1], paste(sort(valid$base), collapse = ", "))
            stop(msg, call. = FALSE)
        } else {
            if (isTRUE(type %in% valid$insight)) {
                base <- valid$base[match(type, valid$insight)]
                insi <- type
            } else {
                base <- type
                insi <- valid$insight[match(type, valid$base)]
            }
            out <- stats::setNames(base, insi)
        }
    # everything else gets a pass (but should be included in the dictionary eventually)
    } else {
        out <- stats::setNames(type, rep(NA, length(type)))
    }


    return(out)
}
