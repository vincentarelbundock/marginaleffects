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
#' vector or a data.frame with two columns called `term` and `estimate`.
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
#' # b1, b2, ... shortcuts can be used to identify the position of the
#' # parameters of interest in the output of FUN
#' deltamethod(mod, "b2 = b3")
#' 
#' # term names with special characters have to be enclosed in backticks
#' deltamethod(mod, "`factor(cyl)6` = `factor(cyl)8`")
#' 
#' mod2 <- lm(mpg ~ hp * drat, data = mtcars)
#' deltamethod(mod2, "`hp:drat` = drat")
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
            colnames(param)[1:2] <- c("term", "estimate")
            return(param)
        }
        if (is.null(hypothesis)) {
            out <- FUNinner(model, ...)
            class(out) <- c("deltamethod", class(out))
            attr(out, "model") <- model
            attr(out, "model_type") <- class(model)[1]
            attr(out, "vcov") <- vcov
            attr(out, "vcov.type") <- vcov.type
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
                estimate = out)
        }

        if (!inherits(out, "data.frame") || any(!c("term", "estimate") %in% colnames(out))) {
            msg <- format_msg(
            "`FUN` must return a numeric vector or a data.frame with two columns named `term` and `estimate`.")
            stop(msg, call. = FALSE)
        }

        out <- get_hypothesis(out, hypothesis = hypothesis, column = "estimate")$estimate
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
        hyplab <- attr(hypothesis, "label")
        if (!is.null(hyplab)) {
            out <- data.frame(
                term = hyplab,
                estimate = b,
                std.error = se)
        }  else {
            out <- data.frame(
                term = "custom",
                estimate = b,
                std.error = se)
        }
    } else {
        out <- data.frame(
            term = paste0("b", seq_along(b)),
            estimate = b,
            std.error = se)
    }

    out <- get_ci(
        out,
        conf_level = conf_level,
        # sometimes get_predicted fails on SE but succeeds on CI (e.g., betareg)
        df = NULL,
        vcov = vcov,
        overwrite = FALSE,
        draws = NULL,
        estimate = "estimate")

    class(out) <- c("deltamethod", class(out))
    attr(out, "model") <- model
    attr(out, "model_type") <- class(model)[1]
    attr(out, "vcov") <- vcov
    attr(out, "vcov.type") <- vcov.type

    return(out)
}
