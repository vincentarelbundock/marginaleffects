get_modeldata <- function(model, ...) {
    out <- insight::get_data(model, verbose = FALSE, additional_variables = TRUE)
    out <- unpack_matrix_1col(out)
    if (is.null(out)) {
        return(NULL)
    }
    data.table::setDF(out)
    return(out)
}
