#' EXPERIMENTAL Uncertainty Estimates for `marginaleffects` Objects
#'
#' @description
#' Warning: This function is experimental. It may be renamed, the user interface may change, or the functionality may migrate to arguments in other `marginaleffects` functions.
#'
#' Apply this function to a model object to change the inferential strategy used to compute uncertainty estimates: delta method or simulation-based inference.
#'
#' @inheritParams slopes
#' @param method String
#' + "delta": delta method standard errors
#' + "boot" package
#' + "rsample" package
#' + "simulation" from a multivariate normal distribution (Krinsky & Robb, 1986)
#' @param R Number of simulations.
#' @param conf_type String: type of bootstrap interval to construct. 
#' + `boot`: "perc", "norm", "basic", or "bca"
#' + `rsample`: "perc" or "bca"
#' @param ... Other arguments are ignored.
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
#' A "decorated" model with additional information on how to conduct inference and compute uncertainty estimates. This decorated model can then fed to any of the `marginaleffects` functions, as one would for any other supported models.
#' @examples
#' library(marginaleffects)
#' library(magrittr)
#' mod <- glm(vs ~ hp * wt + factor(gear), data = mtcars, family = binomial)
#'
#' mod %>%
#'     inferences() %>%
#'     avg_predictions(by = "gear")
#'
#' mod %>%
#'     inferences() %>%
#'     slopes() %>%
#'     head()
#'
#' mod %>%
#'     inferences() %>%
#'     avg_slopes() %>%
#'     posterior_draws("rvar")
#'
#' @export
inferences <- function(model, method = "simulation", R = 1000, conf_type = "perc", ...) {
    checkmate::assert_choice(method, choices = c("delta", "boot", "rsample", "simulation"))
    # delta method requires no decoration, because it is default
    out <- model
    # {boot} package
    if (method == "boot") {
        insight::check_if_installed("boot")
        class(out) <- c("inferences_boot", class(model))
        attr(out, "boot_args") <- c(list(R = R), list(...))
        attr(out, "conf_type") <- conf_type

    } else if (method == "rsample") {
        insight::check_if_installed("rsample")
        class(out) <- c("inferences_rsample", class(model))
        attr(out, "boot_args") <- c(list(times = R), list(...))
        attr(out, "conf_type") <- conf_type

    } else if (method == "simulation") {
        # simulation-based inference
        insight::check_if_installed("MASS")
        out <- model
        class(out) <- c("inferences_simulation", class(out))
        # do this here so we can eventually expand to other functions
        attr(out, "simulate") <- function(R, B, V) MASS::mvrnorm(R, mu = B, Sigma = V)
        attr(out, "R") <- R
    }

    return(out)
}