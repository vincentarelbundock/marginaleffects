#' Estimate and Standard Error of a Non-Linear Function of Estimated Model Parameters
#'
#' `deltamethod` is a function to get a first-order approximate standard error
#' for a nonlinear function of a vector of random variables with known or
#' estimated covariance matrix. [`deltamethod`] emulates the behavior of the
#' excellent and well-established [car::deltaMethod] and [car::linearHypothesis]
#' functions, but it supports more models, requires fewer dependencies, and
#' offers some convenience features like shortcuts for robust standard errors.
#'
#' @inheritParams comparisons
#' @param FUN a function which accepts a model object and returns a numeric
#' vector or a data.frame with two columns called `term` and `value`.
#' @examples
#' library(marginaleffects)
#' mod <- lm(mpg ~ hp + wt + factor(cyl), data = mtcars)
#' 
#' # When `FUN` and `hypothesis` are `NULL`, `deltamethod()` returns a data.frame of parameters
#' deltamethod(mod)
#' 
#' # Test of equality between coefficients
#' deltamethod(mod, "hp = wt")
#' 
#' # Non-linear function
#' deltamethod(mod, "exp(hp + wt) = 0.1")
#' 
#' # Robust standard errors
#' deltamethod(mod, "hp = wt", vcov = "HC3")
#' 
#' # r1, r2, ... shortcuts can be used to identify rows in the output of FUN
#' deltamethod(mod, "r2 = r3")
#' 
#' # term names with special characters have to be enclosed in backticks
#' deltamethod(mod, "`factor(cyl)6` = `factor(cyl)8`")
#' 
#' # The `FUN` argument can be used to compute standard errors for fitted values
#' mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
#' 
#' f <- function(x) predict(x, type = "link", newdata = mtcars)
#' p <- deltamethod(mod, FUN = f)
#' head(p)
#' 
#' f <- function(x) predict(x, type = "response", newdata = mtcars)
#' p <- deltamethod(mod, FUN = f)
#' head(p)
#' 
#' @export
deltamethod <- function(
    model,
    hypothesis = NULL,
    FUN = NULL,
    vcov = NULL,
    conf_level = 0.95,
    ...) {

    vcov <- get_vcov(model = model, vcov = vcov)
    vcov.type <- get_vcov_label(vcov = vcov)
    hypothesis <- sanitize_hypothesis(hypothesis)

    if (is.null(FUN)) {
        FUNinner <- function(model, ...) {
            param <- insight::get_parameters(model, ...)
            colnames(param)[1:2] <- c("term", "value")
            return(param)
        }
        if (is.null(hypothesis)) {
            out <- FUNinner(model, ...)
            return(out)
        }
    } else {
        FUNinner <- FUN
    }

    FUNouter <- function(model, hypothesis) {
        out <- FUNinner(model)

        if (isTRUE(checkmate::check_numeric(out))) {
            out <- data.frame(
                term = seq_along(out),
                value = out)
        }

        if (!inherits(out, "data.frame") || any(!c("term", "value") %in% colnames(out))) {
            msg <- format_msg(
            "`FUN` must return a numeric vector or a data.frame with two columns named `term` and `value`.")
            stop(msg, call. = FALSE)
        }

        out <- get_hypothesis(out, hypothesis = hypothesis, column = "value")$value
        return(out)
    }

    b <- FUNouter(model = model, hypothesis = hypothesis)

    se <- get_se_delta(
        model = model,
        vcov = vcov,
        hypothesis = hypothesis,
        FUN = FUNouter,
        ...)

    if (!is.null(hypothesis)) {
        out <- data.frame(
            term = attr(hypothesis, "label"),
            estimate = b,
            std.error = se)
    } else {
        out <- data.frame(
            term = paste0("r", seq_along(b)),
            estimate = b,
            std.error = se)
    }

    out <- get_ci(
        out,
        conf_level = conf_level,
        # sometimes get_predicted fails on SE but succeeds on CI (e.g., betareg)
        df = NULL,
        overwrite = FALSE,
        draws = NULL,
        estimate = "estimate")

    return(out)
}
