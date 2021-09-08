sanity_dydx_model <- function(model) {
    supported <- c("lm", "glm", "betareg", "clm", "multinom", "lmerMod",
                   "loess", "glmerMod", "fixest", "polr", "ivreg")
    if (!any(supported %in% class(model))) {
        msg <- "Models of class %s are not supported. Supported model classes include: %s."
        msg <- sprintf(msg, class(model)[1], paste(supported, paste = ", "))
        stop(msg)
    }
    return(model)
}


sanity_dydx_group_names <- function (model) {
    group_models <- c("multinom", "polr")
    if (any(group_models %in% class(model))) {
        group_names <- levels(insight::get_response(model))
    } else {
        group_names <- "main"
    }
    return(group_names)
}

    
sanity_dydx_at <- function(model, newdata, at) {
    if (!is.null(at) && !is.null(newdata)) {
        stop("The `at` and `newdata` parameters cannot be used simultaneously.")
    }
    if (!is.null(at)) {
        # sanity checks on `at` are conducted in the `counterfactual` function.
        # This reduces code duplication because that function is useful elsewhere.
        newdata <- counterfactual(model, at = at)
        return(newdata)
    } else {
        return(newdata)
    }
}


sanity_dydx_newdata <- function(model, newdata) {
    checkmate::check_data_frame(newdata, 
                                null.ok = TRUE, 
                                any.missing = FALSE)
    if (is.null(newdata)) {
        newdata <- insight::get_data(model)
    }
    return(newdata)
}


sanity_dydx_variables <- function(model, newdata, variables) {
    checkmate::assert_character(variables, null.ok = TRUE)

    # guess variables
    if (is.null(variables)) {
        variables <- insight::find_variables(model)$conditional
    }

    # subset of numeric variables
    idx <- sapply(newdata[, variables, drop = FALSE], is.numeric)
    variables_numeric <- variables[idx]
    variables_notnumeric <- variables[!idx]
    if (length(variables_notnumeric) > 0) {
        msg <- "No marginal effect was computed for these variables because they are not numeric: %s. Specify the `variables` argument manually to silence this warning."
        warning(sprintf(msg, paste(variables_notnumeric, collapse = ", ")))
    }
    variables <- variables_numeric

    return(variables)
}


sanity_dydx_variance <- function(model, variance) {
    if (!is.null(variance)) {
        unsupported <- c("clm", "lmerMod", "glmerMod", "loess", "polr")
        msg <- "Variance estimates are not yet supported for objects of class %s. Set `variance=NULL` to silence this warning."
        if (any(unsupported %in% class(model))) {
            warning(sprintf(msg, class(model))[1])
            variance <- NULL
        }
    }
    return(variance)
}


sanity_dydx_prediction_type <- function(model, prediction_type) {
    checkmate::assert_character(prediction_type, len = 1, null.ok = FALSE)

    if ("clm" %in% class(model)) {
        if (prediction_type %in% c("prob", "probs")) {
            prediction_type <- "prob"
        } else {
            warning(sprintf('The only `prediction_type` supported for models of class `%s` is `"probs"`. The value of the argument was adjusted automatically. Modify the argument manually to silence this warning.', class(model)[1]))
            prediction_type <- "prob"
        }
    }

    if (any(c("multinom", "nnet") %in% class(model))) {
        if(prediction_type %in% c("prob", "probs")) {
            prediction_type <- "probs"
        } else {
            stop(sprintf('The only `prediction_type` supported for models of class `%s` is `"probs"`. The value of the argument was adjusted automatically. Modify the argument manually to silence this warning.', class(model)[1]))
            prediction_type <- "probs"
        }
    }

    return(prediction_type)
}
