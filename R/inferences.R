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
#' + "conformal_full": prediction intervals using full conformal prediction (see Lei et al., 2018). **Warning**: This method is computationally expensive and typically much slower than split or CV+ methods.
#' @param R Number of resamples, simulations, or cross-validation folds.
#' @param conf_type String: type of bootstrap interval to construct.
#' + `boot`: "perc", "norm", "basic", or "bca"
#' + `fwb`: "perc", "norm", "wald", "basic", "bc", or "bca"
#' + `rsample`: "perc" or "bca"
#' + `simulation`: "perc" or "wald"
#' @param conformal_calibration Data frame of calibration data for split conformal prediction (`method="conformal_split`).
#' @param conformal_train Data frame of training data for full conformal prediction (`method="conformal_full"`). Only required for models where the training data cannot be extracted (e.g., tidymodels workflows).
#' @param conformal_score String. Warning: The `type` argument in `predictions()` must generate predictions which are on the same scale as the outcome variable. Typically, this means that `type` must be "response" or "probs".
#'   + "residual_abs" or "residual_sq" for regression tasks (numeric outcome)
#'   + "softmax" for classification tasks (when `predictions()` returns a `group` columns, such as multinomial or ordinal logit models.
#' @param estimator Function that accepts a data frame, fits a model, applies a `marginaleffects` function, and returns the object. Only supported with `method = "rsample"` or `method = "boot"`. When `method = "rsample"`, the output must include a "term" column. This is not always the case for `predictions()`, in which case users may have to create the column manually.
#' @param ...
#' + If `method = "boot"`, additional arguments are passed to `boot::boot()`.
#' + If `method = "fwb"`, additional arguments are passed to `fwb::fwb()`.
#' + If `method = "rsample"`, additional arguments are passed to `rsample::bootstraps()`, unless the user supplies a `group` argument, in which case all arguments are passed to `rsample::group_bootstraps()`.
#' + If `method = "conformal_full"`, additional arguments control the optimization process:
#'   - `var_multiplier`: multiplier for initial search bounds (default: 10)
#'   - `max_iter`: maximum iterations for root finding (default: 100)
#'   - `tolerance`: tolerance for root finding convergence (default: `.Machine$double.eps^0.25`)
#' + If `method = "conformal_quantile"`, additional arguments are passed to `quantregForest::quantregForest()` for fitting the quantile regression forest (e.g., `ntree`, `mtry`, `nodesize`, `nthreads`).
#' + Additional arguments are ignored for other conformal methods (`conformal_split`, `conformal_cv+`).
#' @details
#' When `method = "simulation"`, we conduct simulation-based inference following the method discussed in Krinsky & Robb (1986):
#' 1. Draw `R` sets of simulated coefficients from a multivariate normal distribution with mean equal to the original model's estimated coefficients and variance equal to the model's variance-covariance matrix (classical, "HC3", or other).
#' 2. Use the `R` sets of coefficients to compute `R` sets of estimands: predictions, comparisons, slopes, or hypotheses.
#' 3. Take quantiles of the resulting distribution of estimands to obtain a confidence interval (when `conf_type = "perc"`) and the standard deviation of simulated estimates to estimate the standard error (which is used for a Z-test and Wald confidence intervals when `conf_type = "wald"`).
#'
#' When `method = "fwb"`, drawn weights are supplied to the model fitting function's `weights` argument; if the model doesn't accept non-integer weights, this method should not be used. If weights were included in the original model fit, they are extracted by [weights()] and multiplied by the drawn weights. These weights are supplied to the `wts` argument of the estimation function (e.g., `comparisons()`).
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
#' Lei, Jing, Max G'Sell, Alessandro Rinaldo, Ryan J. Tibshirani, and Larry Wasserman. 2018. "Distribution-Free Predictive Inference for Regression." Journal of the American Statistical Association 113 (523): 1094–1111.
#'
#' @template parallel
#' @return
#' A `marginaleffects` object with simulation or bootstrap resamples and objects attached.
#' @examples
#' \dontrun{
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
#' }
#' @export
inferences <- function(
    x,
    method,
    R = 1000,
    conf_type = "perc",
    conformal_calibration = NULL,
    conformal_train = NULL,
    conformal_score = "residual_abs",
    estimator = NULL,
    ...) {
    mfx <- attr(x, "marginaleffects")

    # dummy mfx for `estimator` and no marginaleffects object
    if (!inherits(mfx, "marginaleffects_internal")) {
        mfx <- new_marginaleffects_internal(NULL, call("predictions"))
    }


    x <- sanitize_estimator(x = x, estimator = estimator, method = method)

    # we cannot support custom classes because they do not come with `update()` method.
    if (
        !inherits(x, c("hypotheses", "slopes", "comparisons", "predictions")) && !identical(class(x)[1], "data.frame")
    ) {
        sanity_model_supported_class(x, custom = FALSE)
    }

    # inherit conf_level from the original object
    conf_level <- mfx@conf_level

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
            "conformal_cv+",
            "conformal_quantile",
            "conformal_full"
        )
    )

    checkmate::assert(
        checkmate::check_class(x, "predictions"),
        checkmate::check_class(x, "comparisons"),
        checkmate::check_class(x, "slopes"),
        checkmate::check_class(x, "hypotheses")
    )

    if (inherits(mfx@model, "Learner")) {
        msg <- "The inferences() function does not support models of this class."
        stop_sprintf(msg)
    }
    if (inherits(mfx@model, c("model_fit", "workflow"))) {
        if (method %in% c("boot", "rsample", "simulation")) {
            msg <- "Bootstrap and simulation-based inferences are not supported for models of this class."
            stop_sprintf(msg)
        }
    }

    # Issue #1501: `newdata` should only use the pre-evaluated `newdata` instead of bootstrapping datagrid()
    mfx <- attr(x, "marginaleffects")
    call_mfx <- mfx@call

    # Update call with pre-evaluated newdata if available
    if (!is.null(call_mfx)) {
        nd <- mfx@newdata
        # avoid error on two arguments matching `newdata`
        if (inherits(nd, "data.frame") && !"newdata" %in% ...names()) {
            call_mfx[["newdata"]] <- nd
        }

        # Update mfx object if available
        if (!is.null(mfx) && inherits(nd, "data.frame")) {
            mfx@newdata <- nd
            attr(x, "marginaleffects") <- mfx
        }
    }

    if (method == "conformal_split") {
        conformal_fun <- conformal_split
    }
    if (method == "conformal_cv+") {
        conformal_fun <- conformal_cv_plus
    }
    if (method == "conformal_quantile") {
        conformal_fun <- conformal_quantile
    }
    if (method == "conformal_full") {
        conformal_fun <- conformal_full
    }

    # default standard errors are Delta anyway
    if (method == "delta") {
        return(x)
    }

    if (method == "boot") {
        out <- inferences_boot(x, R = R, conf_level = conf_level, conf_type = conf_type, estimator = estimator, mfx = mfx, ...)
    } else if (method == "fwb") {
        if (
            isTRUE("wts" %in% names(mfx@call)) &&
                !isFALSE(mfx@call[["wts"]])
        ) {
            stop_sprintf(
                "The `fwb` method is not supported with the `wts` argument."
            )
        }
        out <- inferences_fwb(x, R = R, conf_level = conf_level, conf_type = conf_type, mfx = mfx, ...)
    } else if (method == "rsample") {
        out <- inferences_rsample(x, R = R, conf_level = conf_level, conf_type = conf_type, estimator = estimator, mfx = mfx, ...)
    } else if (method == "simulation") {
        out <- inferences_simulation(x, R = R, conf_level = conf_level, conf_type = conf_type, mfx = mfx, ...)
    } else if (isTRUE(grepl("conformal", method))) {
        # Conformal prediction sanity checks
        checkmate::assert_class(x, "predictions")
        checkmate::assert_choice(
            conformal_score,
            choices = c("residual_abs", "residual_sq", "softmax")
        )

        # Deprecation warning for conformal_test if passed via ...
        if ("conformal_test" %in% names(list(...))) {
            insight::format_warning(
                "The `conformal_test` argument is deprecated and will be ignored.
Conformal methods now use the `newdata` supplied to `predictions()` automatically."
            )
        }

        # Use newdata from predictions() as test set
        test_data <- mfx@newdata
        if (!inherits(test_data, "data.frame") || nrow(test_data) == 0) {
            stop_sprintf(
                "Conformal prediction requires a valid `newdata` argument in `predictions()`.
Please call predictions(model, newdata = your_test_data) before using inferences()."
            )
        }

        # Method-specific sanity checks
        if (method == "conformal_split") {
            checkmate::assert_data_frame(conformal_calibration, null.ok = FALSE)
        }

        if (method == "conformal_cv+") {
            checkmate::assert_integerish(R, upper = 25)
        }

        if (method == "conformal_quantile") {
            checkmate::assert_data_frame(conformal_calibration, null.ok = FALSE)
        }

        if (method == "conformal_full" && conformal_score == "softmax") {
            stop_sprintf(
                "The 'softmax' score is not supported for full conformal prediction.
Use 'residual_abs' or 'residual_sq' for regression tasks."
            )
        }

        out <- conformal_fun(
            x,
            R = R,
            conf_level = conf_level,
            test = test_data,
            calibration = conformal_calibration,
            train = conformal_train,
            score = conformal_score,
            mfx = mfx,
            ...
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
        "nchains",
        "vcov.type",
        "variables",
        "comparison"
    )
    for (n in attrs) {
        if (n %in% names(attributes(x))) {
            attr(out, n) <- attr(x, n)
        }
    }

    return(out)
}
