# input: character vector or named list
# output: named list of lists where each element represents a variable with: name, value, function, label
sanitize_variables <- function(variables,
                               model,
                               newdata,
                               transform_pre = NULL,
                               by = NULL,
                               interaction = FALSE,
                               contrast_numeric = 1,
                               contrast_factor = "reference") {

    checkmate::assert(
        checkmate::check_character(variables, min.len = 1, null.ok = TRUE, names = "unnamed"),
        checkmate::check_list(variables, names = "unique"),
        combine = "or")

    modeldata <- attr(newdata, "newdata_modeldata")

    # rename to avoid overwriting in case we need info later
    predictors <- variables
    others <- NULL

    if (is.null(newdata)) {
        newdata <- hush(insight::get_data(model))
    }

    # all variable names
    if (!is.null(model)) {
        predictors_all <- insight::find_variables(model, flatten = TRUE)
        # unsupported by insight (e.g., numpyro)
        if (length(predictors_all) == 0) {
            predictors_all <- colnames(newdata)
        }
    } else {
        predictors_all <- colnames(newdata)
    }

    # variables is NULL: all variable names from model
    if (is.null(predictors)) {
        # mhurdle names the variables weirdly
        if (inherits(model, "mhurdle")) {
            predictors <- insight::find_predictors(model, flatten = TRUE)
        } else {
            predictors <- insight::find_variables(model)
        }
        known <- c("fixed", "conditional")
        if (any(known %in% names(predictors))) {
            predictors <- unlist(predictors[known], recursive = TRUE, use.names = FALSE)
        # sometimes triggered by multivariate brms models where we get nested
        # list: predictors$gear$hp
        } else {
            predictors <- unlist(predictors, recursive = TRUE, use.names = FALSE)
        }
    }

    # variable classes: compute only once
    variable_class <- attr(newdata, "newdata_variable_class")

    # variables is character vector: convert to named list
    if (isTRUE(checkmate::check_character(predictors))) {
        predictors_new <- list()
        for (v in predictors) {
            if (isTRUE(variable_class[[v]] == "numeric")) {
                # binary variables: we take the difference by default
                if (!is.null(modeldata[[v]]) && all(modeldata[[v]] %in% 0:1)) {
                    predictors_new[[v]] <- 0:1
                } else {
                    predictors_new[[v]] <- contrast_numeric
                }
            } else {
                predictors_new[[v]] <- contrast_factor
            }
        }
        predictors <- predictors_new
    }

    # check validity of elements of predictors list
    for (v in names(predictors)) {
        if (v %in% colnames(newdata)) {
            if (identical(variable_class[v], "numeric")) {
                sanity_contrast_numeric(predictors[[v]])
            }
            if (isTRUE(variable_class[v] %in% c("factor", "character"))) {
                sanity_contrast_factor(predictors[[v]])
            }
        }
    }

    miss <- setdiff(names(predictors), colnames(newdata))
    if (length(miss) > 0) {
        msg <- format_msg(sprintf(
        "These variables were not found: %s. 
        Try specifying the `newdata` argument explicitly.",
        paste(miss, collapse = ", ")))
    }
    predictors <- predictors[!names(predictors) %in% miss]

    # sometimes `insight` returns interaction component as if it were a constituent variable
    idx <- !grepl(":", names(predictors))
    predictors <- predictors[idx]

    # reserved keywords
    reserved <- intersect(
        names(predictors),
        c("rowid", "group", "term", "contrast", "estimate", "std.error", "statistic", "conf.low", "conf.high"))
    if (isTRUE(length(reserved) > 0)) {
        predictors <- predictors[!names(predictors) %in% reserved]
        msg <- format_msg(sprintf(
        "The following variable names are forbidden to avoid conflicts with the column 
        names of the outputs produced by the `marginaleffects` package: %s
        Please rename your variables before fitting the model or specify the `variables` argument.",
        paste(reserved, collapse = ", ")))
        warning(msg, call. = FALSE)
    }

    # matrix variables are not supported
    mc <- attr(newdata, "newdata_matrix_columns")
    if (length(mc) > 0) {
        predictors <- predictors[!names(predictors) %in% mc]
        msg <- format_msg("Matrix columns are not supported.")
        warning(msg, call. = FALSE)
    }

    # anything left?
    if (length(predictors) == 0) {
        stop("There is no valid predictor variable. Please change the `variables` argument or supply a new data frame to the `newdata` argument.", call. = FALSE)
    }
    others <- setdiff(predictors_all, names(predictors))

    # sometimes weights don't get extracted by `find_variables()`
    w <- tryCatch(insight::find_weights(model), error = function(e) NULL)
    w <- intersect(w, colnames(newdata))
    others <- c(others, w)

    # goals:
    # allow multiple function types: marginaleffects() uses both difference and dydx
    # when transform_pre is defined, use that if it works or turn back to defaults
    # predictors list elements: name, value, function, label

    if (is.null(transform_pre)) {
        fun_numeric <- fun_categorical <- transform_pre_function_dict[["difference"]]
        lab_numeric <- lab_categorical <- transform_pre_label_dict[["difference"]]

    } else if (is.function(transform_pre)) {
        fun_numeric <- fun_categorical <- transform_pre
        lab_numeric <- lab_categorical <- "custom"

    } else if (is.character(transform_pre)) {
        # switch to the avg version when there is a `by` function
        if (isTRUE(checkmate::check_character(by)) && !isTRUE(grepl("avg$", transform_pre))) {
            transform_pre <- paste0(transform_pre, "avg")
        }

        # weights if user requests `avg` or automatically switched
        if (isTRUE(grepl("avg$", transform_pre)) && "marginaleffects_wts_internal" %in% colnames(newdata)) {
            transform_pre <- paste0(transform_pre, "wts")
        }

        fun_numeric <- fun_categorical <- transform_pre_function_dict[[transform_pre]]
        lab_numeric <- lab_categorical <- transform_pre_label_dict[[transform_pre]]
        if (isTRUE(grepl("dydxavg|eyexavg|dyexavg|eydxavg", transform_pre))) {
            fun_categorical <- transform_pre_function_dict[["differenceavg"]]
            lab_categorical <- transform_pre_label_dict[["differenceavg"]]
        } else if (isTRUE(grepl("dydx$|eyex$|dyex$|eydx$", transform_pre))) {
            fun_categorical <- transform_pre_function_dict[["difference"]]
            lab_categorical <- transform_pre_label_dict[["difference"]]
        }

    } else {
        github_issue()
    }

    for (v in names(predictors)) {
        if (isTRUE(variable_class[v] == "numeric")) {
            sanity_contrast_numeric(predictors[[v]])
            fun <- fun_numeric
            lab <- lab_numeric
        } else {
            sanity_contrast_factor(predictors[[v]])
            fun <- fun_categorical
            lab <- lab_categorical
        }
        predictors[[v]] <- list(
            "name" = v,
            "function" = fun,
            "label" = lab,
            "value" = predictors[[v]],
            "transform_pre" = transform_pre)
    }

    # interaction: get_contrasts() assumes there is only one function when interaction=TRUE
    if (isTRUE(interaction)) {
        for (p in predictors) {
            flag <- !identical(p[["function"]], predictors[[1]][["function"]])
            if (flag) {
                stop("When `interaction=TRUE` all variables must use the same contrast function.",
                     call. = FALSE)
            }
        }
    }

    # output
    out <- list(conditional = predictors, others = others)

    return(out)
}

