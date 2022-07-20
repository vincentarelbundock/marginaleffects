get_data_attributes <- function(model = NULL) {
    if (is.null(model)) {
        return(NULL)
    }
    dat <- hush(insight::get_data(model))
    mc <- Filter(function(x) is.matrix(dat[[x]]), colnames(dat))
    cl <- Filter(function(x) is.character(dat[[x]]), colnames(dat))
    cl <- lapply(dat[, cl], unique)
    out <- list(
        "matrix_columns" = mc,
        "character_levels" = cl)
    return(out)
}
