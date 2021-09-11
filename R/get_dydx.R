#' Compute marginal effects (internal function)
#'
#' @rdname get_dydx
#' @inheritParams marginaleffects
#' @inheritParams get_dydx_and_se
#' @keywords internal
#' @export
get_dydx <- function (model, ...) {
    UseMethod("get_dydx", model)
}

#' @rdname get_dydx
#' @export
get_dydx.default <- function(model, 
                             variable,
                             fitfram = insight::get_data(model), 
                             group_name = NULL,
                             prediction_type = "response",
                             numDeriv_method = "simple",
                             ...) {
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- get_predict(model = model,
                            newdata = fitfram_tmp,
                            prediction_type = prediction_type,
                            ...)
        # numDeriv expects a vector
        if (is.matrix(pred) && (!is.null(group_name) && group_name != "main")) {
            pred <- pred[, group_name, drop = TRUE]
        }
        # strip weird attributes added by some methods (e.g., predict.svyglm)
        pred <- as.numeric(pred)
        return(pred)
    }
    g <- numDeriv::grad(func = inner, 
                        x = fitfram[[variable]], 
                        method = numDeriv_method)
    return(g)
}
