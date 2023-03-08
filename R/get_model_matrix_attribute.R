get_model_matrix_attribute <- function(model, newdata = NULL) {
    # supported models (no inheritance)
    if (!isTRUE(class(model)[1] %in% c("lm", "glm"))) {
        return(newdata)
    }
    
    # we don't support offsets, so revert to stats::predict()
    if (!is.null(model[["offset"]])) {
        return(newdata)
    }

    # subset variables for listwise deletion
    vars <- unlist(insight::find_predictors(model), use.names = FALSE)
    vars <- c(vars, unlist(insight::find_response(model), use.names = FALSE))
    vars <- intersect(vars, colnames(newdata))
    MM <- hush(insight::get_modelmatrix(model, data = data.frame(newdata)[, vars]))
    
    # sometimes there are still mismatches due to listwise deletion or missing factor levels
    beta <- get_coef(model)
    if (isTRUE(nrow(MM) == nrow(newdata)) && ncol(MM) == length(beta)) {
        attr(newdata, "marginaleffects_model_matrix") <- MM
    }

    return(newdata)
}