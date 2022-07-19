# input: character vector or named list
# output: named list of lists where each element represents a variable with: name, value, function, label
sanitize_variables <- function(variables,
                               model,
                               newdata,
                               transform_pre = NULL,
                               interaction = FALSE,
                               contrast_numeric = 1,
                               contrast_factor = "reference") {

    if (is.null(newdata)) {
        newdata <- hush(insight::get_data(model))
    }
    newdata <- sanitize_newdata(model = model, newdata = newdata)
    checkmate::assert_data_frame(newdata, min.row = 1, null.ok = TRUE)
    checkmate::assert(
        checkmate::check_character(variables, min.len = 1, null.ok = TRUE, names = "unnamed"),
        checkmate::check_list(variables, names = "unique"),
        combine = "or")

    # rename to avoid overwriting in case we need info later
    predictors <- variables
    others <- NULL

    # all variable names
    if (!is.null(model)) {
        predictors_all <- insight::find_variables(model, flatten = TRUE)
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
        # sometimes triggered by multivariate brms models where we get nested list: predictors$gear$hp
        } else {
            predictors <- unlist(predictors, recursive = TRUE, use.names = FALSE)
        }
    }

    # variable classes: compute only once
    variables_class <- list()
    if (isTRUE(checkmate::check_character(predictors))) {
        idx <- predictors
    } else if (isTRUE(checkmate::check_list(predictors))) {
        idx <- names(predictors)
    }
    for (v in idx) {
        variables_class[[v]] <- tryCatch(
            find_variable_class(v, newdata = newdata, model = model),
            error = function(e) NULL)
    }

    # variables is character vector: convert to named list
    if (isTRUE(checkmate::check_character(predictors))) {
        predictors_new <- list()
        for (v in predictors) {
            if (isTRUE(variables_class[[v]] == "numeric")) {
                predictors_new[[v]] <- contrast_numeric
            } else {
                predictors_new[[v]] <- contrast_factor
            }
        }
        predictors <- predictors_new
    }

    # check validity of elements of predictors list
    for (n in names(predictors)) {
        if (n %in% colnames(newdata)) {
            if (identical(variables_class[v], "numeric")) {
                sanity_contrast_numeric(predictors[[v]])
            }
            if (isTRUE(variables_class[v] %in% c("factor", "character"))) {
                sanity_contrast_factor(predictors[[v]])
            }
        }
    }

    # check missing variables
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
    idx <- names(predictors) %in% attr(newdata, "matrix_columns")
    if (any(idx)) {
        predictors <- predictors[!idx]
        msg <- format_msg("Matrix columns are not supported.")
        warning(msg, call. = FALSE)
    }

    # anything left?
    if (length(predictors) == 0) {
        stop("There are no valid predictors. Please change the `variables` argument.", call. = FALSE)
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
        fun_numeric <- fun_categorical <- transform_pre_function_dict[[transform_pre]]
        lab_numeric <- lab_categorical <- transform_pre_label_dict[[transform_pre]]
        if (isTRUE(transform_pre %in% c("dydx", "eyex", "eydx", "dyex"))) {
            fun_categorical <- transform_pre_function_dict[["difference"]]
            lab_categorical <- transform_pre_label_dict[["difference"]]
        } 
    } else {
        github_issue()
    }

    for (v in names(predictors)) {
        if (isTRUE(variables_class[v] == "numeric")) {
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
            "value" = predictors[[v]])
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

    # save character levels
    # Character variables are treated as factors by model-fitting functions,
    # but unlike factors, they do not not keep a record of all factor levels.
    # This poses problem when feeding `newdata` to `predict`, which often
    # breaks (via `model.matrix`) when the data does not include all possible
    # factor levels.
    levels_character <- list()
    for (v in names(predictors)) {
        origindata <- hush(insight::get_data(model))
        if (v %in% names(predictors)) {
            levels_character[[v]] <- unique(origindata[[v]])
        }
    }

    # output
    out <- list(conditional = predictors, others = others, levels = levels_character)

    return(out)
}

