align_J_V <- function(J, V) {
    if (ncol(J) == ncol(V)) {
        out <- list(J = J, V = V)
    } else {
        cols <- intersect(colnames(J), colnames(V))
        if (length(cols) == 0) {
            stop("The Jacobian does not match the variance-covariance matrix.", call. = FALSE)
        }
        V <- V[cols, cols]
        J <- J[, cols, drop = FALSE]
        out <- list(J = J, V = V)
    } 
    return(out)
}
