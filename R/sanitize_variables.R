# input: character vector or named list
# output: named list of lists where each element represents a variable with: name, value, function, label
sanitize_variables <- function(variables,
                               model,
                               newdata,
                               transform_pre = NULL,
                               contrast_numeric = 1,
                               contrast_factor = "reference") {

    if (is.null(newdata)) {
        newdata <- hush(insight::get_data(model))
    }
    newdata <- sanitize_newdata(model = model, newdata = newdata)
    checkmate::assert_data_frame(newdata, min.row = 1, null.ok = TRUE)
    checkmate::assert(
        checkmate::check_character(variables, min.len = 1, null.ok = TRUE),
        checkmate::check_list(variables, names = "unique"),
        combine = "or")

    # rename to avoid overwriting in case we need info later
    predictors <- variables
    others <- NULL

    # all variables
    if (!is.null(model)) {
        tmp <- insight::find_variables(model)
        predictors_all <- unlist(tmp, recursive = TRUE, use.names = FALSE)

        # variables is NULL: all variable names from model
        if (is.null(predictors)) {
            # mhurdle names the variables weirdly
            if (inherits(model, "mhurdle")) {
                predictors <- insight::find_predictors(model, flatten = TRUE)
            } else {
                predictors <- unlist(tmp[c("fixed", "conditional")], recursive = TRUE, use.names = FALSE)
            }
        }
    } else {
        predictors <- predictors_all <- colnames(newdata)
    }

    # variable classes: compute only once
    variables_class <- list()
    if (isTRUE(checkmate::check_character(predictors))) {
        idx <- predictors
    } else if (isTRUE(checkmate::check_list(predictors))) {
        idx <- names(predictors)
    }
    for (v in idx) {
        variables_class[[v]] <- find_variable_class(v, newdata = newdata, model = model)
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

    # predictors list elements: name, value, function, label
    for (v in names(predictors)) {
        # `variables` character input has two possibilities:
        if (isTRUE(checkmate::check_character(predictors[[v]]))) {
            # 1. transform_pre function shortcut. this takes precedence over transform_pre
            if (predictors[[v]] %in% names(transform_pre_function_dict)) {
                tmp <- list(
                    "name" = v,
                    "function" = transform_pre_function_dict[[predictors[[v]]]],
                    "label" = transform_pre_label_dict[[predictors[[v]]]],
                    "value" = NULL)
            # 2. transform_pre gap value shortcut
            } else {
                # transform_pre can be a custom function
                if (is.function(transform_pre)) {
                    fun <- transform_pre
                    lab <- "custom"
                } else if (is.character(transform_pre)) {
                    fun <- transform_pre_function_dict[[transform_pre]]
                    lab <- transform_pre_label_dict[[transform_pre]]
                } else {
                    fun <- transform_pre_function_dict[[predictors[[v]]]]
                    lab <- transform_pre_label_dict[[predictors[[v]]]]
                }
                tmp <- list(
                    "name" = v,
                    "function" = fun,
                    "label" = lab,
                    "value" = predictors[[v]])
            }

        # `variables` is numeric or other, means it's the value not the function
        } else {
            # transform_pre can be a custom function
            if (is.function(transform_pre)) {
                fun <- transform_pre
                lab <- "custom"
            } else if (is.character(transform_pre)) {
                fun <- transform_pre_function_dict[[transform_pre]]
                lab <- transform_pre_label_dict[[transform_pre]]
            } else {
                fun <- transform_pre_function_dict[["difference"]]
                lab <- transform_pre_label_dict[["difference"]]
            }
            tmp <- list(
                "name" = v,
                "function" = fun,
                "label" = lab,
                "value" = predictors[[v]])
        }

        predictors[[v]] <- tmp
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
