#' Get the dataset used to fit a model
#'
#' Mostly for internal use, but can be useful because the output is consistent across model classes.
#' @inheritParams slopes
#' @return A data frame of the original data used to fit the model, or `NULL` if not available.
#' @rdname get_modeldata
#' @export
get_modeldata <- function(model, ...) {
    modeldata <- insight::get_data(model, verbose = FALSE, additional_variables = TRUE)
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
