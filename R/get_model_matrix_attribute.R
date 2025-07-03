get_model_matrix_attribute <- function(model, newdata = NULL) {
    # supported models (no inheritance)
    if (!isTRUE(class(model)[1] %in% c("lm", "glm", "rq"))) {
        return(newdata)
    }

    # stats::model.matrix creates all-0 columns with splines::bs() and other functions
    # this may be too aggressive, but it avoids all functions
    flag <- any(grepl("\\(", setdiff(names(get_coef(model)), "(Intercept)")))
    funs <- grep("\\(", names(get_coef(model)), value = TRUE)
    funs <- funs[!grepl("factor\\(|\\(Intercept", funs)]
    if (length(funs) > 0) {
        return(newdata)
    }

    # we don't support offsets, so revert to stats::predict()
    if (!is.null(model[["offset"]])) {
        return(newdata)
    }

    # subset variables for listwise deletion
    dv <- insight::find_response(model)
    vars <- unlist(
        insight::find_predictors(model, verbose = FALSE),
        use.names = FALSE
    )
    vars <- c(vars, unlist(dv, use.names = FALSE))
    vars <- intersect(vars, colnames(newdata))

    nd <- as.data.frame(newdata)[, vars, drop = FALSE]

    MM <- hush(get_model_matrix(model, newdata = nd))

    attr(newdata, "marginaleffects_model_matrix") <- MM
    return(newdata)
}
