#' check type sanity
#'
#' @param model model object
#' @param type character vector
#' @return Named vector where value is the Base R `type` and name is the
#' insight::get_predicted `predict`
#' @noRd
sanity_type <- function(model, type) {

    checkmate::assert_character(type, min.len = 1, null.ok = FALSE)

    model_class <- class(model)[1]

    dict <- type_dictionary

    # known models are scrutinized tightly
    if (model_class %in% dict$class) {
        valid <- dict[dict$class == model_class, ]
        if (!all(type %in% c(valid$base, valid$insight))) {
            msg <- sprintf("The `type` argument for models of class `%s` must be an element of: %s",
                           class(model)[1], paste(sort(valid$base), collapse = ", "))
            stop(msg)
        } else {
            base_from_insight <- valid$base[match(type, valid$insight)]
            base <- ifelse(is.na(base_from_insight), type, base_from_insight)
            insi <- valid$insight[match(base, valid$base)]
            out <- stats::setNames(base, insi)
        }
    # everything else gets a pass (but should be included in the dictionary eventually)
    } else {
        out <- stats::setNames(type, rep(NA, length(type)))
    }

    return(out)
}
