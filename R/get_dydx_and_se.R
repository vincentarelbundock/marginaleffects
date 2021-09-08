#' @title Get dY/dX
#' @rdname get_dydx_and_se
#' @export
get_dydx_and_se <- function (model, ...) {
    UseMethod("get_dydx_and_se", model)
}

#' @rdname get_dydx_and_se
#' @export
get_dydx_and_se.default <- function(model, 
                                   fitfram, 
                                   variable, 
                                   variance, 
                                   group_name = NULL,
                                   prediction_type = "response",
                                   numDeriv_method = "simple", 
                                   ...) {

    # marginal effects
    g <- get_dydx(model = model,
                 fitfram = fitfram,
                 variable = variable,
                 group_name = group_name,
                 prediction_type = prediction_type,
                 numDeriv_method = numDeriv_method)

    out <- data.frame(rowid = 1:nrow(fitfram), 
                      term = variable,
                      dydx = g)
    out$group <- group_name # could be NULL

    # standard errors
    if (!is.null(variance)) {

        # special case: polr (TODO: generalize using a get_vcov.polr() method)
        if (any(c("polr", "betareg") %in% class(model)) && !is.null(group_name)) {
            variance <- variance[names(stats::coef(model)), names(stats::coef(model))]
        }

        se <- get_se_delta(model = model,
                           fitfram = fitfram,
                           variable = variable,
                           variance = variance,
                           group_name = group_name,
                           prediction_type = prediction_type,
                           numDeriv_method = numDeriv_method)
        out$std.error <- se
    }

    return(out)
}
