#' @title Get dY/dX
#' @rdname get_dydx
#' @export
get_dydx <- function (model, ...) {
    UseMethod("get_dydx", model)
}

#' @rdname get_dydx
#' @export
get_dydx.default <- function(model, 
                             fitfram, 
                             variable, 
                             variance, 
                             group_name = NULL,
                             prediction_type = "response",
                             numDeriv_method = "simple", 
                             ...) {
    # marginal effects
    g <- get_gradient(model = model,
                      fitfram = fitfram,
                      variable = variable,
                      group_name = group_name,
                      prediction_type = prediction_type,
                      numDeriv_method = numDeriv_method)
    out <- data.frame(dydx = g)
    out$group <- group_name

    # standard errors
    if (!is.null(variance)) {
        se <- get_jacobian(model = model,
                           fitfram = fitfram,
                           variable = variable,
                           variance = variance,
                           group_name = group_name,
                           prediction_type = prediction_type,
                           numDeriv_method = numDeriv_method)
        out$std.error <- se
    }

    # output
    out$rowid <- 1:nrow(fitfram)
    return(out)
}
