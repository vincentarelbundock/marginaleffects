# Variable sanitization orchestrator and comparison configuration

get_comparison_functions <- function(mfx) {
    # goals:
    # allow multiple function types: slopes() uses both difference and dydx
    # when comparison is defined, use that if it works or turn back to defaults
    # predictors list elements: name, value, function, label
    comparison <- mfx@comparison
    by <- mfx@by

    if (is.null(comparison)) {
        fun_numeric <- fun_categorical <- comparison_function_dict[["difference"]]
        lab_numeric <- lab_categorical <- comparison_label_dict[["difference"]]
    } else if (is.function(comparison)) {
        fun_numeric <- fun_categorical <- comparison
        lab_numeric <- lab_categorical <- "custom"
    } else if (is.character(comparison)) {
        # switch to the avg version when there is a `by` function
        if (
            (isTRUE(checkmate::check_character(by)) || isTRUE(by)) &&
                !isTRUE(grepl("avg$", comparison))
        ) {
            comparison <- paste0(comparison, "avg")
        }

        # weights if user requests `avg` or automatically switched
        if (
            isTRUE(grepl("avg$", comparison)) &&
                "marginaleffects_wts_internal" %in% colnames(mfx@newdata)
        ) {
            comparison <- paste0(comparison, "wts")
        }

        fun_numeric <- fun_categorical <- comparison_function_dict[[comparison]]
        lab_numeric <- lab_categorical <- comparison_label_dict[[comparison]]
        if (isTRUE(grepl("dydxavgwts|eyexavgwts|dyexavgwts|eydxavgwts", comparison))) {
            fun_categorical <- comparison_function_dict[["differenceavgwts"]]
            lab_categorical <- comparison_label_dict[["differenceavgwts"]]
        } else if (isTRUE(grepl("dydxavg|eyexavg|dyexavg|eydxavg", comparison))) {
            fun_categorical <- comparison_function_dict[["differenceavg"]]
            lab_categorical <- comparison_label_dict[["differenceavg"]]
        } else if (isTRUE(grepl("dydx$|eyex$|dyex$|eydx$", comparison))) {
            fun_categorical <- comparison_function_dict[["difference"]]
            lab_categorical <- comparison_label_dict[["difference"]]
        }
    }

    list(
        fun_numeric = fun_numeric,
        fun_categorical = fun_categorical,
        lab_numeric = lab_numeric,
        lab_categorical = lab_categorical,
        comparison = comparison
    )
}

add_functions_and_labels <- function(predictors, comparison_config, mfx) {
    modeldata <- mfx@modeldata

    for (v in names(predictors)) {
        if (
            (check_variable_class(mfx, v, "numeric") || check_variable_class(mfx, v, "integer")) &&
                !check_variable_class(mfx, v, "binary")
        ) {
            fun <- comparison_config$fun_numeric
            lab <- comparison_config$lab_numeric
        } else {
            fun <- comparison_config$fun_categorical
            lab <- comparison_config$lab_categorical
        }
        predictors[[v]] <- list(
            "name" = v,
            "function" = fun,
            "label" = lab,
            "value" = predictors[[v]],
            "comparison" = comparison_config$comparison
        )
    }
    predictors
}

add_epsilon_values <- function(predictors, mfx, eps) {
    modeldata <- mfx@modeldata

    # epsilon for finite difference
    for (v in names(predictors)) {
        if (!is.null(eps)) {
            predictors[[v]][["eps"]] <- eps
        } else if (is.numeric(modeldata[[v]])) {
            predictors[[v]][["eps"]] <- 1e-4 *
                diff(range(modeldata[[v]], na.rm = TRUE, finite = TRUE))
            # 1-row grid has 0 range
            if (predictors[[v]][["eps"]] == 0) {
                predictors[[v]][["eps"]] <- 1e-4
            }
        } else {
            predictors[[v]]["eps"] <- list(NULL)
        }
    }
    predictors
}

sanitize_internal_variables <- function(predictors, mfx, model, calling_function) {
    # can't take the slope of an outcome, except in weird brms models (issue #1006)
    if (!inherits(model, "brmsfit") || !isTRUE(length(model$formula$forms) > 1)) {
        dv <- mfx@variable_names_response
        # sometimes insight doesn't work
        if (length(dv) > 0) {
            predictors <- predictors[setdiff(names(predictors), dv)]
        }
    }
    if (length(predictors) == 0) {
        stop_sprintf(
            "There is no valid predictor variable. Please make sure your model includes predictors and use the `variables` argument."
        )
    }

    # sort variables alphabetically
    predictors <- predictors[sort(names(predictors))]

    # internal variables are not predictors
    predictors <- predictors[
        !names(predictors) %in% c("marginaleffects_wts_internal", "rowid_dedup")
    ]
    if (length(predictors) == 0 && calling_function == "comparisons") {
        stop_sprintf("No valid predictor variable.")
    }

    predictors
}

# input: character vector or named list
# output: named list of lists where each element represents a variable with: name, value, function, label
add_variables <- function(
    variables,
    mfx) {
    # Input validation
    checkmate::assert(
        checkmate::check_null(variables),
        checkmate::check_character(variables, min.len = 1, names = "unnamed"),
        checkmate::check_list(variables, min.len = 1, names = "unique"),
        combine = "or"
    )

    # Extract and normalize predictors
    predictors <- get_predictors(variables, mfx)
    predictors <- sanitize_predictor_container(predictors)

    # Handle predictions-specific processing
    if (mfx@calling_function == "predictions") {
        modeldata <- mfx@modeldata
        predictors <- add_prediction_functions(predictors, modeldata)
        predictors <- add_numeric_shortcuts(predictors, mfx)
    }

    # Set defaults and validate
    modeldata <- mfx@modeldata
    predictors <- add_default_values(predictors, mfx)
    predictors <- sanitize_predictor_specs(predictors, mfx)

    # Configure comparison functions and labels
    comparison_config <- get_comparison_functions(mfx)
    predictors <- add_functions_and_labels(predictors, comparison_config, mfx)

    # Add epsilon values
    predictors <- add_epsilon_values(predictors, mfx, mfx@eps)

    # Final sanitization
    predictors <- sanitize_internal_variables(predictors, mfx, mfx@model, mfx@calling_function)

    # output
    mfx@variables <- predictors
    return(mfx)
}
