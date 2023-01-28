simulation_inference <- function(model, iter = 1000, ...) {
    insight::check_if_installed("MASS")
    out <- model
    class(out) <- c("simulation_inference", class(out))
    # do this here so we can eventually expand to other functions
    attr(out, "simulate") <- function(iter, B, V) MASS::mvrnorm(iter, mu = B, Sigma = V)
    attr(out, "iter") <- iter
    return(out)
}