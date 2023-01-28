#' Simulation-based inference
#' 
#' @description
#' Simulation-based inference using the 
#' @inheritParams slopes
#' @param method "simulation" or "delta"
#' @param ... Other arguments are ignored.
#' @section References:
#' 
#' * Krinsky, I., and A. L. Robb. 1986. “On Approximating the Statistical Properties of Elasticities.” Review of Economics and Statistics 68 (4): 715–9. 
#' * Dowd, Bryan E., William H. Greene, and Edward C. Norton. "Computation of standard errors." Health services research 49.2 (2014): 731-750.
#' * King, Gary, Michael Tomz, and Jason Wittenberg. "Making the most of statistical analyses: Improving interpretation and presentation." American journal of political science (2000): 347-361
#' 
#' @return
#' A "decorated" model with additional information on how to conduct inference and compute uncertainty estimates. This decorated model can be used in any of the `marginaleffects` functions, as all other supported models. 
#' @examples
#' library(marginaleffects)
#' library(magrittr)
#' mod <- glm(vs ~ hp * wt + factor(gear), data = mtcars, family = binomial)
#' 
#' mod %>% inference() %>%
#'         avg_predictions(by = "gear")
#' 
#' mod %>% inference() %>%
#'         slopes() %>%
#'         head()
#' 
#' mod %>% inference() %>%
#'         avg_slopes() %>%
#'         posterior_draws("rvar")
#' 
inference <- function(model, method = "simulation", iter = 1000, ...) {
    checkmate::assert_choices(method, choices = c("simulation", "delta"))

    # delta method
    if (method == "delta") return(model) 

    # simulation-based inference
    insight::check_if_installed("MASS")
    out <- model
    class(out) <- c("inference_simulation", class(out))
    # do this here so we can eventually expand to other functions
    attr(out, "simulate") <- function(iter, B, V) MASS::mvrnorm(iter, mu = B, Sigma = V)
    attr(out, "iter") <- iter
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.inference_simulation <- function(x, newdata, vcov = FALSE, ...) {
    coefmat <- attr(x, "coefmat")
    # coefmat: BxM 
    checkmate::assert_matrix(coefmat)
    # remove the special class to avoid calling myself
    mod <- x
    class(mod) <- setdiff(class(mod), "simulation_inference")
    FUN <- function(coefs) {
        mod_tmp <- set_coef(mod, coefs = coefs)
        get_predict(mod_tmp, newdata = newdata)$estimate
    }
    # should never compute SE via delta method for these models
    out <- get_predict(mod, newdata = newdata, vcov = FALSE, ...)
    attr(out, "posterior_draws") <- apply(coefmat, MARGIN = 1, FUN = FUN)
    return(out)
}


#' @rdname get_vcov
#' @export
get_vcov.inference_simulation <- function(x, ...) return(NULL)


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.inference_simulation <- function(model, vcov = FALSE, ...) {
    tmp <- model
    class(tmp) <- setdiff(class(tmp), "simulation_inference")
    B <- get_coef(tmp)
    V <- get_vcov(tmp, vcov = vcov)
    attr(model, "coefmat") <- attr(model, "simulate")(iter = attr(model, "iter"), B = B, V = V)
    attr(model, "V") <- V
    attr(model, "B") <- B
    return(model)
}