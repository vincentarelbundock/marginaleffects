#' check type sanity
#'
#' @param model model object
#' @param type character vector
#' @noRd
sanitize_type <- function(model, type) {
    checkmate::assert_character(type, len = 1, null.ok = TRUE)
    cl <- class(model)[1]
    if (!cl %in% type_dictionary$class) {
        cl <- "other"
    }
    dict <- type_dictionary[type_dictionary$class == cl, , drop = FALSE]
    checkmate::assert_choice(type, choices = dict$type, null.ok = TRUE)
    if (is.null(type)) {
        type <- dict$type[1]
    }
    return(type)
}