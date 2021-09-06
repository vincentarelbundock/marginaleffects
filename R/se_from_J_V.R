se_from_J_V <- function(J, V) {
    # Var(dydx) = J Var(beta) J'
    # computing the full matrix is memory-expensive, and we only need the diagonal
    # computation trick: https://stackoverflow.com/a/42569902/342331
    A <- J %*% V
    V <- colSums(t(A) * t(J))
    out <- sqrt(V)
    return(out)
}
