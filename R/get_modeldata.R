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
