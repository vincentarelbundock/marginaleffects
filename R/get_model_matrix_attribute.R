get_model_matrix_attribute <- function(model, newdata = NULL) {
    # supported models (no inheritance)
    if (!isTRUE(class(model)[1] %in% c("lm", "glm", "rq"))) {
        return(newdata)
    }

    # stats::model.matrix creates all-0 columns with splines::bs() and other functions
    # this may be too aggressive, but it avoids all functions
    flag <- any(grepl("\\(", setdiff(names(get_coef(model)), "(Intercept)")))
    if (isTRUE(flag)) {
        return(newdata)
    }

    # we don't support offsets, so revert to stats::predict()
    if (!is.null(model[["offset"]])) {
        return(newdata)
    }

    # subset variables for listwise deletion
    vars <- unlist(
        insight::find_predictors(model, verbose = FALSE),
        use.names = FALSE
    )
    vars <- c(vars, unlist(insight::find_response(model), use.names = FALSE))
    vars <- intersect(vars, colnames(newdata))

    MM <- hush(get_model_matrix(model, newdata = data.frame(newdata)[, vars]))

    attr(newdata, "marginaleffects_model_matrix") <- MM
    return(newdata)
}
