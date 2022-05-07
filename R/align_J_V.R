align_J_V <- function(J, V) {
    cols <- intersect(colnames(J), colnames(V))
    if (length(cols) == 0) {
        stop("The Jacobian does not match the variance-covariance matrix.")
    }
    V <- V[cols, cols]
    J <- J[, cols, drop = FALSE]
    out <- list(V = V, J = J)
    return(out)
}
