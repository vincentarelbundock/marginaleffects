#' (EXPERIMENTAL) Bootstrap, Conformal, and Simulation-Based Inference
#'
#' @description
#' Warning: This function is experimental. It may be renamed, the user interface may change, or the functionality may migrate to arguments in other `marginaleffects` functions.
#'
#' Apply this function to a `marginaleffects` object to change the inferential method used to compute uncertainty estimates.
#'
#' @param x Object produced by one of the core `marginaleffects` functions, or a data frame suitable for the function supplied to the `estimator` argument.
#' @param method String
#' + "delta": delta method standard errors
#' + "boot" package
#' + "fwb": fractional weighted bootstrap
#' + "rsample" package
#' + "simulation" from a multivariate normal distribution (Krinsky & Robb, 1986)
#' + "mi" multiple imputation for missing data
#' + "conformal_split": prediction intervals using split conformal prediction (see Angelopoulos & Bates, 2022)
#' + "conformal_cv+": prediction intervals using cross-validation+ conformal prediction (see Barber et al., 2020)
#' @param R Number of resamples, simulations, or cross-validation folds.
#' @param conf_type String: type of bootstrap interval to construct.
#' + `boot`: "perc", "norm", "basic", or "bca"
#' + `fwb`: "perc", "norm", "basic", "bc", or "bca"
#' + `rsample`: "perc" or "bca"
#' + `simulation`: argument ignored.
#' @param conformal_test Data frame of test data for conformal prediction.
#' @param conformal_calibration Data frame of calibration data for split conformal prediction (`method="conformal_split`).
#' @param conformal_score String. Warning: The `type` argument in `predictions()` must generate predictions which are on the same scale as the outcome variable. Typically, this means that `type` must be "response" or "probs".
#'   + "residual_abs" or "residual_sq" for regression tasks (numeric outcome)
#'   + "softmax" for classification tasks (when `predictions()` returns a `group` columns, such as multinomial or ordinal logit models.
#' @param estimator Function that accepts a data frame, fits a model, applies a marginaleffects function, and returns the object. Only supported with method="rsample" or method="boot". When method="rsample", the output must include a "term" column. This is not always the case for predictions(), in which case users may have to create the column manually.
#' @param ...
#' + If `method="boot"`, additional arguments are passed to `boot::boot()`.
#' + If `method="fwb"`, additional arguments are passed to `fwb::fwb()`.
#' + If `method="rsample"`, additional arguments are passed to `rsample::bootstraps()`.
#' + Additional arguments are ignored for all other methods.
#' @details
#' When `method="simulation"`, we conduct simulation-based inference following the method discussed in Krinsky & Robb (1986):
#' 1. Draw `R` sets of simulated coefficients from a multivariate normal distribution with mean equal to the original model's estimated coefficients and variance equal to the model's variance-covariance matrix (classical, "HC3", or other).
#' 2. Use the `R` sets of coefficients to compute `R` sets of estimands: predictions, comparisons, slopes, or hypotheses.
#' 3. Take quantiles of the resulting distribution of estimands to obtain a confidence interval and the standard deviation of simulated estimates to estimate the standard error.
#'
#' When `method="fwb"`, drawn weights are supplied to the model fitting function's `weights` argument; if the model doesn't accept non-integer weights, this method should not be used. If weights were included in the original model fit, they are extracted by [weights()] and multiplied by the drawn weights. These weights are supplied to the `wts` argument of the estimation function (e.g., `comparisons()`).
#'
#' Warning: custom model classes are not supported by `inferences()` because they are not guaranteed to come with an appropriate `update()` method.
#'
#' @section References:
#'
#' Krinsky, I., and A. L. Robb. 1986. "On Approximating the Statistical Properties of Elasticities." Review of Economics and Statistics 68 (4): 715–9.
#'
#' King, Gary, Michael Tomz, and Jason Wittenberg. "Making the most of statistical analyses: Improving interpretation and presentation." American journal of political science (2000): 347-361
#'
#' Dowd, Bryan E., William H. Greene, and Edward C. Norton. "Computation of standard errors." Health services research 49.2 (2014): 731-750.
#'
#' Angelopoulos, Anastasios N., and Stephen Bates. 2022. "A Gentle Introduction to Conformal Prediction and Distribution-Free Uncertainty Quantification." arXiv. https://doi.org/10.48550/arXiv.2107.07511.
#'
#' Barber, Rina Foygel, Emmanuel J. Candes, Aaditya Ramdas, and Ryan J. Tibshirani. 2020. "Predictive Inference with the Jackknife+." arXiv. http://arxiv.org/abs/1905.02928.
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
#'     inferences(method = "boot")
#'
#' avg_predictions(mod, by = "Species") %>%
#'     inferences(method = "rsample")
#'
#' # Fractional (bayesian) bootstrap
#' avg_slopes(mod, by = "Species") %>%
#'     inferences(method = "fwb") %>%
#'     get_draws("rvar") %>%
#'     data.frame()
#'
#' # Simulation-based inference
#' slopes(mod) %>%
#'     inferences(method = "simulation") %>%
#'     head()
#'
#' # Two-step estimation procedure: Propensity score + G-Computation
#' lalonde <- get_dataset("lalonde")
#' estimator <- function(data) {
#'     # Step 1: Estimate propensity scores
#'     fit1 <- glm(treat ~ age + educ + race, family = binomial, data = data)
#'     ps <- predict(fit1, type = "response")
#'     # Step 2: Fit weighted outcome model
#'     m <- lm(re78 ~ treat * (re75 + age + educ + race),
#'         data = data, weight = ps
#'     )
#'     # Step 3: Compute average treatment effect by G-computation
#'     avg_comparisons(m, variables = "treat", wts = ps, vcov = FALSE)
#' }
#' inferences(lalonde, method = "rsample", estimator = estimator)
#'
#' }
#' @export
inferences <- function(
    x,
    method,
    R = 1000,
    conf_type = "perc",
    conformal_test = NULL,
    conformal_calibration = NULL,
    conformal_score = "residual_abs",
    estimator = NULL,
    ...
) {
    if (inherits(attr(x, "model"), c("model_fit", "workflow"))) {
        msg <- "The `inferences()` function does not support `tidymodels` objects."
        stop_sprintf(msg)
    }

    # we cannot support custom classes because they do not come with `update()` method.
    sanity_model_supported_class(x, custom = FALSE)

    x <- sanitize_estimator(x = x, estimator = estimator, method = method)

    # inherit conf_level from the original object
    conf_level <- attr(x, "conf_level")
    if (is.null(conf_level)) {
        conf_level <- 0.95
    }

    checkmate::assert_number(conf_level, lower = 1e-10, upper = 1 - 1e-10)
    checkmate::assert_integerish(R, lower = 2)
    checkmate::assert_choice(
        method,
        choices = c(
            "delta",
            "boot",
            "fwb",
            "rsample",
            "simulation",
            "conformal_split",
            "conformal_cv+"
        )
    )

    checkmate::assert(
        checkmate::check_class(x, "predictions"),
        checkmate::check_class(x, "comparisons"),
        checkmate::check_class(x, "slopes"),
        checkmate::check_class(x, "hypotheses")
    )

    # Issue #1501: `newdata` should only use the pre-evaluated `newdata` instead of bootstrapping datagrid()
    call_mfx <- attr(x, "call")
    if (!is.null(call_mfx)) {
        nd <- attr(x, "newdata")
        if (inherits(nd, "data.frame")) {
            call_mfx[["newdata"]] <- nd
        }
        attr(x, "call") <- call_mfx
    }

    if (method == "conformal_split") {
        conformal_fun <- conformal_split
    }
    if (method == "conformal_cv+") {
        conformal_fun <- conformal_cv_plus
    }

    # default standard errors are Delta anyway
    if (method == "delta") {
        return(x)
    }

    if (method == "boot") {
        insight::check_if_installed("boot")
        out <- inferences_boot(x, R = R, conf_level = conf_level, conf_type = conf_type, estimator = estimator, ...)
    } else if (method == "fwb") {
        insight::check_if_installed("fwb")
        dots <- list(...)
        if (!"verbose" %in% names(dots)) {
            dots[["verbose"]] <- FALSE
        }
        if (
            isTRUE("wts" %in% names(attr(x, "call"))) &&
                !isFALSE(attr(x, "call")[["wts"]])
        ) {
            insight::format_error(
                "The `fwb` method is not supported with the `wts` argument."
            )
        }
        out <- inferences_fwb(x, R = R, conf_level = conf_level, conf_type = conf_type, ...)
    } else if (method == "rsample") {
        insight::check_if_installed("rsample")
        out <- inferences_rsample(x, R = R, conf_level = conf_level, conf_type = conf_type, estimator = estimator, ...)
    } else if (method == "simulation") {
        insight::check_if_installed("MASS")
        out <- inferences_simulation(x, R = R, conf_level = conf_level, ...)
    } else if (isTRUE(grepl("conformal", method))) {
        out <- conformal_fun(
            x,
            R = R,
            conf_level = conf_level,
            test = conformal_test,
            calibration = conformal_calibration,
            score = conformal_score
        )
    }

    # Preserve specific attributes from the input object
    attrs <- c(
        "conf_level",
        "by",
        "lean",
        "type",
        "newdata",
        "call",
        "model_type",
        "model",
        "jacobian",
        "vcov",
        "weights",
        "transform",
        "hypothesis_by",
        "nchains",
        "vcov.type",
        "variables",
        "comparison",
        "comparison_label",
        "transform_label"
    )
    for (n in attrs) {
        if (!is.null(attr(x, n))) {
            attr(out, n) <- attr(x, n)
        }
    }

    return(out)
}
