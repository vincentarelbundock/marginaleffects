#' Compute marginal effects (internal function)
#'
#' @rdname get_dydx_continuous
#' @inheritParams marginaleffects
#' @inheritParams get_dydx_and_se
#' @keywords internal
#' @return Numeric vector of marginal effects associated to a continuous regressor
get_dydx_continuous <- function (model, ...) {
    UseMethod("get_dydx_continuous", model)
}

#' @rdname get_dydx_continuous
get_dydx_continuous.default <- function(model, 
                                        variable,
                                        fitfram = insight::get_data(model), 
                                        group_name = NULL,
                                        type = "response",
                                        numDeriv_method = "simple",
                                        ...) {
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- get_predict(model = model,
                            newdata = fitfram_tmp,
                            type = type,
                            group_name = group_name,
                            ...)

        # strip weird attributes added by some methods (e.g., predict.svyglm)
        pred <- as.numeric(pred)
        return(pred)
    }
    g <- numDeriv::grad(func = inner, 
                        x = fitfram[[variable]], 
                        method = numDeriv_method)
    out <- data.frame(rowid = 1:nrow(fitfram),
                      term = variable,
                      dydx = g)
    return(out)
}
