#' @title Get Gradient
#' @rdname get_gradient
#' @export
get_gradient <- function (model, ...) {
    UseMethod("get_gradient", model)
}

#' @rdname get_gradient
#' @export
get_gradient.default <- function(model, 
                                 fitfram, 
                                 variable, 
                                 group_name = NULL,
                                 prediction_type = "response",
                                 numDeriv_method = "simple",
                                 ...) {
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- stats::predict(model, 
                               newdata = fitfram_tmp, 
                               type = prediction_type)
        # numDeriv expects a vector
        if (is.matrix(pred) && !is.null(group_name)) {
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
