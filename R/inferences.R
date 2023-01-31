#' (EXPERIMENTAL) Bootstrap and Simulation-Based Inference
#'
#' @description
#' Warning: This function is experimental. It may be renamed, the user interface may change, or the functionality may migrate to arguments in other `marginaleffects` functions.
#'
#' Apply this function to a `marginaleffects` object to change the inferential method used to compute uncertainty estimates.
#'
#' @param x Object produced by one of the core `marginaleffects` functions.
#' @param method String
#' + "delta": delta method standard errors
#' + "boot" package
#' + "rsample" package
#' + "simulation" from a multivariate normal distribution (Krinsky & Robb, 1986)
#' @param R Number of resamples or simulations.
#' @param conf_type String: type of bootstrap interval to construct. 
#' + `boot`: "perc", "norm", "basic", or "bca"
#' + `rsample`: "perc" or "bca"
#' + `simulation`: argument ignored.
#' @param ... 
#' + If `method="boot"`, additional arguments are passed to `boot::boot()`.
#' + If `method="rsample"`, additional arguments are passed to `rsample::bootstraps()`.
#' + If `method="simulation"`, additional arguments are ignored.
#' @details
#' When `method="simulation"`, we conduct simulation-based inference following the method discussed in Krinsky & Robb (1986):
#' 1. Draw `R` sets of simulated coefficients from a multivariate normal distribution with mean equal to the original model's estimated coefficients and variance equal to the model's variance-covariance matrix (classical, "HC3", or other).
#' 2. Use the `R` sets of coefficients to compute `R` sets of estimands: predictions, comparisons, or slopes.
#' 3. Take quantiles of the resulting distribution of estimands to obtain a confidence interval and the standard deviation of simulated estimates to estimate the standard error.
#'
#' @section References:
#'
#' Krinsky, I., and A. L. Robb. 1986. “On Approximating the Statistical Properties of Elasticities.” Review of Economics and Statistics 68 (4): 715–9.
#'
#' King, Gary, Michael Tomz, and Jason Wittenberg. "Making the most of statistical analyses: Improving interpretation and presentation." American journal of political science (2000): 347-361
#'
#' Dowd, Bryan E., William H. Greene, and Edward C. Norton. "Computation of standard errors." Health services research 49.2 (2014): 731-750.
#'
#' @return
#' A `marginaleffects` object with simulation or bootstrap resamples and objects attached.
#' @examples
#' library(marginaleffects)
#' library(magrittr)
#' mod <- glm(vs ~ hp * wt + factor(gear), data = mtcars, family = binomial)
#'
#' avg_predictions(mod, by = "gear") %>%
#'   inferences()
#'
#' slopes(mod) %>%
#'   inferences() %>%
#'   head()
#'
#' avg_slopes(mod) %>%
#'   inferences() %>%
#'   posterior_draws("rvar")
#'
#' @export
inferences <- function(x, method = "simulation", R = 1000, conf_type = "perc", ...) {

    checkmate::assert_choice(
        method,
        choices = c("delta", "boot", "rsample", "simulation"))

    mfx_call <- attr(x, "call")
    model <- attr(x, "model")

    if (method == "boot") {
        insight::check_if_installed("boot")
        attr(model, "inferences_method") <- "boot"
        attr(model, "inferences_dots") <- c(list(R = R), list(...))
        attr(model, "inferences_conf_type") <- conf_type

    } else if (method == "rsample") {
        insight::check_if_installed("rsample")
        attr(model, "inferences_method") <- "rsample"
        attr(model, "inferences_dots") <- c(list(times = R), list(...))
        attr(model, "inferences_conf_type") <- conf_type

    } else if (method == "simulation") {
        insight::check_if_installed("MASS")
        attr(model, "inferences_R") <- R
        attr(model, "inferences_simulate") <- function(R, B, V) {
            MASS::mvrnorm(R, mu = B, Sigma = V)
        }
        class(model) <- c("inferences_simulation", class(model))
    }

    mfx_call[["model"]] <- model
    out <- recall(mfx_call)
    return(out)
}


inferences_dispatch <- function(model, FUN, ...) {
    if (isTRUE(attr(model, "inferences_method") == "rsample")) {
        bootstrap_rsample(model = model, FUN = FUN, ...)
    } else if (isTRUE(attr(model, "inferences_method") == "boot")) {
        bootstrap_boot(model = model, FUN = FUN, ...)
    } else {
        return(NULL)
    }
}