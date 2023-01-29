#' EXPERIMENTAL Uncertainty Estimates for `marginaleffects` Objects
#'
#' @description
#' Warning: This function is experimental. It may be renamed, the user interface may change, or the functionality may migrate to arguments in other `marginaleffects` functions.
#'
#' Apply this function to a model object to change the inferential strategy used to compute uncertainty estimates: delta method or simulation-based inference.
#'
#' @inheritParams slopes
#' @param method String: "simulation", "boot", or "delta". See below for references and details.
#' @param R Number of simulations.
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
inferences <- function(model, method = "simulation", R = 10, ...) {
    checkmate::assert_choice(method, choices = c("delta", "boot", "simulation"))
    # delta method requires no decoration, because it is default
    out <- model
    # {boot} package
    if (method == "boot") {
        insight::check_if_installed("boot")
        class(out) <- c("inferences_boot", class(model))
        attr(out, "boot_args") <- c(list(R = R), list(...))
    } else if (method == "simulation") {
        # simulation-based inference
        insight::check_if_installed("MASS")
        out <- model
        class(out) <- c("inference_simulation", class(out))
        # do this here so we can eventually expand to other functions
        attr(out, "simulate") <- function(R, B, V) MASS::mvrnorm(R, mu = B, Sigma = V)
        attr(out, "R") <- R
    }
    return(out)
}


inferences_boot <- function(model, FUN, ...) {
    insight::check_if_installed("boot")
    insight::check_if_installed("broom")
    class(model) <- setdiff(class(model), "inferences_boot")
    modeldata <- get_modeldata(model)
    modcall <- insight::get_call(model)
    data.table::setDF(modeldata)
    dots <- list(...)
    dots[["vcov"]] <- FALSE
    bootfun <- function(data, indices) {
        d <- data[indices, , drop = FALSE]
        modcall[["data"]] <- d
        modboot <- eval(modcall)
        modboot <- eval(modboot)
        args <- c(list(modboot), dots)
        out <- do.call(FUN, args)$estimate
        return(out)
    }
    # no-bootstrap object to return
    out <- do.call(FUN, c(list(model), dots))
    args <- list("data" = modeldata, "statistic" = bootfun)
    args <- c(args, attr(model, "boot_args"))
    args <- args[unique(names(args))]
    B <- do.call(boot::boot, args)
    B$call <- match.call()
    if (is.null(args[["conf_level"]])) {
        conf_level <- .95
    } else {
        conf_level <- args[["conf_level"]]
    }
    tmp <- broom::tidy(B, conf.int = TRUE, conf.level = conf_level)
    colnames(tmp)[colnames(tmp) == "statistic"] <- "estimate"
    for (col in colnames(tmp)) {
        out[[col]] <- tmp[[col]]
    }
    attr(out, "boot") <- B
    return(out)
}



#' @rdname get_predict
#' @export
get_predict.inference_simulation <- function(model, newdata, vcov = FALSE, ...) {
    coefmat <- attr(model, "coefmat")
    # coefmat: BxM 
    checkmate::assert_matrix(coefmat)
    # remove the special class to avoid calling myself
    mod <- model
    class(mod) <- setdiff(class(mod), "inference_simulation")
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
get_vcov.inference_simulation <- function(model, ...) return(NULL)


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.inference_simulation <- function(model, vcov = FALSE, ...) {
    tmp <- model
    class(tmp) <- setdiff(class(tmp), "inference_simulation")
    B <- get_coef(tmp)
    V <- get_vcov(tmp, vcov = vcov)
    attr(model, "coefmat") <- attr(model, "simulate")(R = attr(model, "R"), B = B, V = V)
    attr(model, "V") <- V
    attr(model, "B") <- B
    return(model)
}