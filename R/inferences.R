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
#' + "fwb": fractional weighted bootstrap
#' + "rsample" package
#' + "simulation" from a multivariate normal distribution (Krinsky & Robb, 1986)
#' + "mi" multiple imputation for missing data
#' @param R Number of resamples or simulations.
#' @param conf_type String: type of bootstrap interval to construct.
#' + `boot`: "perc", "norm", "basic", or "bca"
#' + `fwb`: "perc", "norm", "basic", "bc", or "bca"
#' + `rsample`: "perc" or "bca"
#' + `simulation`: argument ignored.
#' @param ...
#' + If `method="boot"`, additional arguments are passed to `boot::boot()`.
#' + If `method="fwb"`, additional arguments are passed to `fwb::fwb()`.
#' + If `method="rsample"`, additional arguments are passed to `rsample::bootstraps()`.
#' + If `method="simulation"`, additional arguments are ignored.
#' @details
#' When `method="simulation"`, we conduct simulation-based inference following the method discussed in Krinsky & Robb (1986):
#' 1. Draw `R` sets of simulated coefficients from a multivariate normal distribution with mean equal to the original model's estimated coefficients and variance equal to the model's variance-covariance matrix (classical, "HC3", or other).
#' 2. Use the `R` sets of coefficients to compute `R` sets of estimands: predictions, comparisons, or slopes.
#' 3. Take quantiles of the resulting distribution of estimands to obtain a confidence interval and the standard deviation of simulated estimates to estimate the standard error.
#'
#' When `method="fwb"`, drawn weights are supplied to the model fitting function's `weights` argument; if the model doesn't accept non-integer weights, this method should not be used. If weights were included in the original model fit, they are extracted by [weights()] and multiplied by the drawn weights. These weights are supplied to the `wts` argument of the estimation function (e.g., `comparisons()`).
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
#' \dontrun{
#' library(marginaleffects)
#' library(magrittr)
#' set.seed(1024)
#' mod <- lm(Sepal.Length ~ Sepal.Width * Species, data = iris)
#'
#' # bootstrap
#' avg_predictions(mod, by = "Species") %>%
#'   inferences(method = "boot")
#'
#' avg_predictions(mod, by = "Species") %>%
#'   inferences(method = "rsample")
#'
#' # Fractional (bayesian) bootstrap
#' avg_slopes(mod, by = "Species") %>%
#'   inferences(method = "fwb") %>%
#'   posterior_draws("rvar") %>%
#'   data.frame()
#'
#' # Simulation-based inference
#' slopes(mod) %>%
#'   inferences(method = "simulation") %>%
#'   head()
#' }
#' @export
inferences <- function(x, method, R = 1000, conf_type = "perc", ...) {

    checkmate::assert_choice(
        method,
        choices = c("delta", "boot", "fwb", "rsample", "simulation"))

    if (!inherits(x, c("predictions", "comparisons", "slopes"))) {
        msg <- sprintf("Objects of class `%s` are not supported by `inferences()`.", class(x)[1])
        insight::format_error(msg)
    }

    mfx_call <- attr(x, "call")
    model <- mfx_call[["model"]]

    # default standard errors are Delta anyway
    if (method == "delta") {
        return(x)

    } else if (method == "boot") {
        insight::check_if_installed("boot")
        attr(model, "inferences_method") <- "boot"
        attr(model, "inferences_dots") <- c(list(R = R), list(...))
        attr(model, "inferences_conf_type") <- conf_type

    } else if (method == "fwb") {
        insight::check_if_installed("fwb")
        dots <- list(...)
        if (!"verbose" %in% names(dots)) {
            dots[["verbose"]] <- FALSE
        }
        attr(model, "inferences_method") <- "fwb"
        attr(model, "inferences_dots") <- c(list(R = R), dots)
        attr(model, "inferences_conf_type") <- conf_type
        if (isTRUE("wts" %in% names(attr(x, "call"))) && !is.null(attr(x, "call")[["wts"]])) {
            insight::format_error('The `fwb` method is not supported with the `wts` argument.')
        }


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
    } else if (isTRUE(attr(model, "inferences_method") == "fwb")) {
        bootstrap_fwb(model = model, FUN = FUN, ...)
    } else {
        return(NULL)
    }
}