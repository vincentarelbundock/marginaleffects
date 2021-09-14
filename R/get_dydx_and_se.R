#' Get marginal effects and standard errors (internal function)
#'
#' @return A data.frame with term names, group names, marginal effects, and
#' standard errors.
#' @param variable A string to identify the variable whose marginal effect to compute.
#' @param fitfram A data.frame over which to compute marginal effects.
#' @param group_name String to identify the "group" or "level" of the terms to
#'   estimate. Groups are often used in models like multinomial logit where each
#'   level of the response variable is associated to its own set of coefficients.
#' @inheritParams marginaleffects
#' @rdname get_dydx_and_se
#' @keywords internal
#' @export
get_dydx_and_se <- function (model, ...) {
    UseMethod("get_dydx_and_se", model)
}

#' @rdname get_dydx_and_se
#' @export
get_dydx_and_se.default <- function(model, 
                                    variable, 
                                    fitfram = insight::get_data(model), 
                                    vcov = stats::vcov(model), 
                                    group_name = NULL,
                                    predict_type = "response",
                                    numDeriv_method = "simple", 
                                    ...) {

    # marginal effects
    g <- get_dydx(model = model,
                  fitfram = fitfram,
                  variable = variable,
                  group_name = group_name,
                  predict_type = predict_type,
                  numDeriv_method = numDeriv_method,
                  ...)

    out <- data.frame(rowid = 1:nrow(fitfram), 
                      term = variable,
                      dydx = g)
    out$group <- group_name # could be NULL

    # unit-level standard errors are slower to compute. When vcov=NULL, use a single typical dataset.
    if (is.null(vcov)) {
        fitfram <- typical(data = fitfram)
        vcov <- try(get_vcov(model), silent = TRUE)
    }

    # special case: polr (TODO: generalize using a get_vcov.polr() method)
    if (inherits(vcov, "matrix") &&
        any(c("polr", "betareg") %in% class(model)) && 
        !is.null(group_name)) {
        vcov <- vcov[names(stats::coef(model)), names(stats::coef(model))]
    }

    se <- try(get_se_delta(model = model,
                           fitfram = fitfram,
                           variable = variable,
                           vcov = vcov,
                           group_name = group_name,
                           predict_type = predict_type,
                           numDeriv_method = numDeriv_method,
                           ...),
              silent = TRUE)
    if (!inherits(se, "try-error")) {
        out$std.error <- se
    }

    return(out)
}
