#' Get or set the dataset used to fit a model
#'
#' `get_modeldata()` retrieves the data used to fit a model. If the data was
#' previously attached via `set_modeldata()`, it is returned directly. Otherwise,
#' the data is retrieved from the model object or the environment using
#' `insight::get_data()`.
#'
#' `set_modeldata()` attaches a dataset to a model object as an attribute. This
#' is useful when the training data may not be available in the environment where
#' `marginaleffects` functions are called, such as inside `lapply()`, in Shiny
#' apps, or in nested function calls. By attaching the data explicitly, you ensure
#' that `marginaleffects` always uses the correct dataset.
#'
#' @inheritParams slopes
#' @param newdata A data frame to attach to the model object.
#' @return `get_modeldata()` returns a data frame of the original data used to
#'   fit the model, or `NULL` if not available. `set_modeldata()` returns the
#'   model with the data attached as an attribute.
#' @rdname get_modeldata
#' @export
get_modeldata <- function(model, ...) {
    # Check for user-supplied data via set_modeldata()
    modeldata <- attr(model, "marginaleffects_modeldata")

    if (is.null(modeldata)) {
        # Fall back to insight, capturing any warnings
        insight_warnings <- NULL
        modeldata <- tryCatch(
            withCallingHandlers(
                insight::get_data(model, verbose = TRUE, additional_variables = TRUE),
                warning = function(w) {
                    insight_warnings <<- c(insight_warnings, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            ),
            error = function(e) NULL
        )
        # If insight raised warnings, re-emit once with recommendation
        if (!is.null(modeldata) && length(insight_warnings) > 0) {
            msg <- paste(
                "The training data could not be extracted reliably from the model object.",
                "Column types, factor levels, or values may have been coerced or altered.",
                "It is safer to use `set_modeldata(model, data)` to attach the training data explicitly.",
                paste("Original warning from `insight::get_data()`:", insight_warnings, collapse = " ")
            )
            warn_once(msg, "marginaleffects_modeldata_from_environment")
        }
    }

    modeldata <- unpack_matrix_1col(modeldata)
    if (is.null(modeldata)) {
        return(NULL)
    }
    # Avoid mutating a data.table returned by reference (would affect the model's
    # original data.table object).
    modeldata <- data.table::copy(modeldata)
    data.table::setDF(modeldata)
    return(modeldata)
}


#' @rdname get_modeldata
#' @export
set_modeldata <- function(model, newdata) {
    checkmate::assert_data_frame(newdata)
    # Deep copy to protect against data.table reference semantics
    attr(model, "marginaleffects_modeldata") <- data.table::copy(newdata)
    model
}
