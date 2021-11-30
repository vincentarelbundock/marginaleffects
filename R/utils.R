find_categorical <- function(x) {
    idx <- sapply(x, function(x)
                  is.character(x) ||
                  is.factor(x) ||
                  is.logical(x) ||
                  isTRUE(attr(x, "factor")))
    out <- colnames(x)[idx]
    return(out)
}
