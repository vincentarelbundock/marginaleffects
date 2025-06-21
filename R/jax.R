jacobian_jax_lm <- function(X, coefs) {
    np_array <- numpy$array
    get_predict_jax_lm <- function(coefs) {
        X %*% coefs
    }
    f_jacobian <- jax$jacfwd(get_predict_jax_lm)
    J <- f_jacobian(np_array(coefs))
    J <- numpy$array(J)
    return(J)
}