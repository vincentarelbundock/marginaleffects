sort_columns <- function(x, first = NULL, alpha = FALSE) {
    cols <- colnames(x)

    if (isTRUE(alpha)) {
        cols <- sort(cols)
    }

    if (!is.null(first)) {
        cols <- unique(c(first, cols))
    }

    cols <- intersect(cols, colnames(x))

    if (inherits(x, "data.table")) {
        out <- x[, ..cols, drop = FALSE]
    } else {
        out <- x[, cols, drop = FALSE]
    }

    return(out)
}