#' check type sanity
#'
#' @param model model object
#' @param type character vector
#' @noRd
sanitize_type <- function(model, type, by = FALSE, hypothesis = NULL, calling_function = "raw") {
    # mlr3
    if (inherits(model, "Learner")) {
        if (is.null(type)) type <- "response"
        valid <- setdiff(model$predict_types, "se")
        checkmate::assert_choice(type, choices = valid, null.ok = TRUE)
        return(type)
    }

    checkmate::assert_character(type, len = 1, null.ok = TRUE)

    if (inherits(model, "model_fit")) {
        cl <- "model_fit"
    } else {
        cl <- class(model)[1]
    }

    # if (!cl %in% type_dictionary$class) {
    #     cl <- "other"
    # }

    dict <- type_dictionary
    # raw is often invoked by `get_predict()`, which is required for {clarify} and others.
    # we only allow invlink(link) in predictions() and marginal_means(), which are handled by {marginaleffects}

    # invlink(link) only supported by predictions()
    if (!isTRUE(calling_function %in% "predictions")) {
        dict <- dict[dict$type != "invlink(link)", , drop = FALSE]
    }

    # invlink(link) only default for predictions() if by=FALSE
    if (!isTRUE(type == "invlink(link)") && !isFALSE(by)) {
        dict <- dict[dict$type != "invlink(link)", , drop = FALSE]
    }

    # fixest: invlink(link) only supported for glm model
    if (inherits(model, "fixest")) {
        if (!isTRUE(hush(model[["method_type"]]) %in% c("feglm"))) {
            dict <- dict[dict$type != "invlink(link)", , drop = FALSE]
        }
    }

    dict <- dict[dict$class == cl, , drop = FALSE]
    if (nrow(dict) > 0) {
        checkmate::assert_choice(type, choices = dict$type, null.ok = TRUE)
        if (is.null(type)) {
            type <- dict$type[1]
        }
    }

    if (is.null(type)) type <- "response"

    if (identical(type, "invlink(link)") && !is.null(hypothesis)) {
        msg <- 'When `type="invlink(link)"`, the `hypothesis` is tested and statistics are computed before back-transformation to the link scale. Make sure you specify the value of `hypothesis` accordingly.'
        warn_once(msg, "marginaleffects_invlink_link_hypothesis_must_be_on_link_scale")
    }

    return(type)
}
