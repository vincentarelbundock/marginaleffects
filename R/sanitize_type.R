#' check type sanity
#'
#' @param model model object
#' @param type character vector
#' @noRd
sanitize_type <- function(model, type, calling_function = NULL) {
    checkmate::assert_character(type, len = 1, null.ok = TRUE)
    cl <- class(model)[1]
    if (!cl %in% type_dictionary$class) {
        cl <- "other"
    }
    dict <- type_dictionary
    if (calling_function %in% c("slopes", "comparisons")) {
        dict <- dict[dict$type != "invlink(link)", , drop = FALSE]
    }

    # fixest: invlink(link) only supported for glm model
    if (inherits(model, "fixest")) {
        if (!isTRUE(hush(model[["method_type"]]) %in% c("feglm"))) {
            dict <- dict[dict$type != "invlink(link)", , drop = FALSE]
        }
    }

    dict <- dict[dict$class == cl, , drop = FALSE]
    checkmate::assert_choice(type, choices = dict$type, null.ok = TRUE)
    if (is.null(type)) {
        type <- dict$type[1]
    }
    return(type)
}