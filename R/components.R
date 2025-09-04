#' @importFrom generics components
#' @export
generics::components


#' Extract components from marginaleffects objects
#'
#' @param object A marginaleffects object (predictions, comparisons, slopes, or hypotheses)
#' @param component Character string specifying which component to extract. Must be a valid
#'   slot name from the internal S4 object. If `NULL` (the default), `components()` prints a message with
#'   all available component names. Common components include: "model", "newdata",
#'   "modeldata", "call", "jacobian", "vcov_model", "type", "by", "comparison", "variables", etc.
#' @param ... Ignored.
#' @return The requested component from the mfx object
#' @details This function provides access to the internal components stored in the `mfx`
#'   attribute of marginaleffects objects. The `mfx` attribute contains an S4 object of
#'   class "marginaleffects_internal" with various slots containing model information,
#'   data, and computational details used by the marginaleffects functions.
#'
#' Warning: the internal slot names are not considered part of the public API and may change
#' without warning in future versions of the marginaleffects package.
#' @export
components.marginaleffects <- function(object, component = NULL, ...) {
    mfx <- attr(object, "marginaleffects", exact = TRUE)

    if (is.null(mfx)) {
        stop("No mfx attribute found in object")
    }

    if (identical(component, "all")) {
        return(mfx)
    }

    # Get all slot names from the S4 object
    valid_components <- sort(methods::slotNames(mfx))

    if (is.null(component)) {
        msg <- checkmate::check_choice(component, choices = valid_components)
        msg <- sub(".*\\{", "The `component` argument must be one of {'all',", msg)
        message(msg)
        return(invisible(NULL))
    }

    checkmate::assert_choice(component, choices = valid_components)

    return(methods::slot(mfx, component))
}

#' @export
components.predictions <- components.marginaleffects

#' @export
components.hypotheses <- components.marginaleffects

#' @export
components.slopes <- components.marginaleffects

#' @export
components.comparisons <- components.marginaleffects
