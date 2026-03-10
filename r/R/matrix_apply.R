matrix_apply_column <- function(x, FUN, by = NULL) {
    if (inherits(by, "data.frame") && nrow(by) == nrow(x)) {
        x <- cbind(data.table::data.table(by), data.table::data.table(x))
        data.table::setDT(x)
        bycols <- colnames(by)
        x <- x[, lapply(.SD, FUN), keyby = bycols]
        cols <- setdiff(colnames(x), colnames(by))
        x <- x[, ..cols]
        x <- as.matrix(x)
        dimnames(x) <- NULL
    } else {
        x <- apply(x, MARGIN = 2, FUN = FUN, simplify = TRUE)
        if (is.null(dim(x))) {
            x <- matrix(x, nrow = 1)
        }
    }
    return(x)
}
