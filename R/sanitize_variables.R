# input: character vector or named list
# output: named list of lists where each element represents a variable with: name, value, function, label
sanitize_variables <- function(variables,
                               model,
                               newdata,
                               transform_pre = NULL,
                               by = NULL,
                               cross = FALSE,
                               contrast_numeric = 1,
                               contrast_factor = "reference",
                               calling_function = "comparisons") {

    checkmate::assert(
        checkmate::check_character(variables, min.len = 1, null.ok = TRUE, names = "unnamed"),
        checkmate::check_list(variables, names = "unique"),
        combine = "or")

    # data
    if (is.null(newdata)) {
        newdata <- hush(insight::get_data(model))
    }

    modeldata <- attr(newdata, "newdata_modeldata")
    if (is.null(modeldata)) {
        modeldata <- newdata
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

    # rename to avoid overwriting
    predictors <- variables

    # variables is NULL: all variable names from model
    if (is.null(predictors)) {
        # mhurdle names the variables weirdly
        if (inherits(model, "mhurdle")) {
            predictors <- insight::find_predictors(model, flatten = TRUE)
        } else {
            predictors <- insight::find_variables(model)
        }
        known <- c("fixed", "conditional", "zero_inflated")
        if (any(known %in% names(predictors))) {
            predictors <- unlist(predictors[known], recursive = TRUE, use.names = FALSE)
        # sometimes triggered by multivariate brms models where we get nested
        # list: predictors$gear$hp
        } else {
            predictors <- unlist(predictors, recursive = TRUE, use.names = FALSE)
        }
    } else {
        predictors <- variables
    }

    # variable classes: compute only once
    variable_class <- attr(newdata, "newdata_variable_class")

    # character -> list
    if (isTRUE(checkmate::check_character(predictors))) {

        # when users includes variables not in the model
        bad <- setdiff(predictors, names(variable_class))
        if (length(bad) > 0) {
            msg <- "These elements should not appear in the `variables` argument: %s."
            msg <- sprintf(msg, paste(bad, collapse = ", "))
            insight::format_error(msg)
        }

        predictors_new <- list()

        for (v in predictors) {

            if (isTRUE(variable_class[[v]] == "numeric")) {

                # binary variables: we take the difference by default
                v_unique <- unique(modeldata[[v]])
                if (all(v_unique %in% 0:1)) {
                    predictors_new[[v]] <- 0:1

                } else if (calling_function == "comparisons") {
                    predictors_new[[v]] <- contrast_numeric

                } else if (calling_function == "predictions") {
                    if (length(v_unique) < 6) {
                        predictors_new[[v]] <- v_unique
                    } else {
                        predictors_new[[v]] <- stats::fivenum(modeldata[[v]])
                    }
                }

            } else {

                if (calling_function == "comparisons") {
                    predictors_new[[v]] <- contrast_factor

                } else if (calling_function == "predictions") {
                    v_unique <- unique(modeldata[[v]])
                    if (length(v_unique) < 6) {
                        predictors_new[[v]] <- v_unique
                    } else {
                        msg <- "Too many unique values in the %v variable. Please specify the desired values explicitly in the `variables` argument."
                        msg <- sprintf(msg, v)
                        insight::format_error(msg)
                    }
                }
            }
        }

        predictors <- predictors_new
    }

    # missing variables
    miss <- setdiff(names(predictors), colnames(newdata))
    predictors <- predictors[!names(predictors) %in% miss]
    if (length(miss) > 0) {
        msg <- sprintf(
            "These variables were not found: %s.  Try specifying the `newdata` argument explicitly.",
            paste(miss, collapse = ", "))
        insight::format_warning(msg)
    }

    # sometimes `insight` returns interaction component as if it were a constituent variable
    idx <- !grepl(":", names(predictors))
    predictors <- predictors[idx]

    # reserved keywords
    reserved <- intersect(
        names(predictors),
        c("rowid", "group", "term", "contrast", "estimate", "std.error", "statistic", "conf.low", "conf.high"))
    if (isTRUE(length(reserved) > 0)) {
        predictors <- predictors[!names(predictors) %in% reserved]
        msg <- sprintf("The following variable names are forbidden to avoid conflicts with the column names of the outputs produced by the `marginaleffects` package: %s Please rename your variables before fitting the model or specify the `variables` argument.", paste(reserved, collapse = ", "))
        insight::format_warning(msg)
    }

    # matrix variables are not supported
    mc <- attr(newdata, "newdata_matrix_columns")
    if (length(mc) > 0 && any(names(predictors) %in% mc)) {
      predictors <- predictors[!names(predictors) %in% mc]
      insight::format_warning("Matrix columns are not supported.")
    }

    # anything left?
    if (length(predictors) == 0) {
        msg <- "There is no valid predictor variable. Please change the `variables` argument or supply a new data frame to the `newdata` argument."
        insight::format_error(msg)
    }
    others <- setdiff(predictors_all, names(predictors))


    # check validity of elements of predictors list
    for (v in names(predictors)) {

        if (v %in% colnames(newdata)) {

            if (identical(variable_class[[v]], "numeric")) {

                if (calling_function == "comparisons") {

                    # TODO
                    # For comparisons(), the string shortcuts are processed in contrast_data_* functions because we need fancy labels.
                    # Eventually it would be nice to consolidate, but that's a lot of work.
                    valid_str <- c("iqr", "minmax", "sd", "2sd")
                    flag <- isTRUE(checkmate::check_numeric(predictors[[v]], min.len = 1, max.len = 2)) ||
                            isTRUE(checkmate::check_choice(predictors[[v]], choices = valid_str)) ||
                            isTRUE(checkmate::check_function(predictors[[v]]))
                    if (!isTRUE(flag)) {
                        msg <- "The %s element of the `variables` argument is invalid."
                        msg <- sprintf(msg, v)
                        insight::format_error(msg)
                    }

                } else if (calling_function == "predictions") {

                    # string shortcuts
                    if (identical(predictors[[v]], "iqr")) {
                        predictors[[v]] <- stats::quantile(modeldata[[v]], probs = c(.25, .75), na.rm = TRUE)
                    } else if (identical(predictors[[v]], "minmax")) {
                        predictors[[v]] <- c(min(modeldata[[v]], na.rm = TRUE), max(modeldata[[v]], na.rm = TRUE))
                    } else if (identical(predictors[[v]], "sd")) {
                        s <- stats::sd(modeldata[[v]], na.rm = TRUE)
                        m <- mean(modeldata[[v]], na.rm = TRUE)
                        predictors[[v]] <- c(m - s / 2, m + s / 2)
                    } else if (identical(predictors[[v]], "2sd")) {
                        s <- stats::sd(modeldata[[v]], na.rm = TRUE)
                        m <- mean(modeldata[[v]], na.rm = TRUE)
                        predictors[[v]] <- c(m - s, m + s)
                    } else if (identical(predictors[[v]], "threenum")) {
                        s <- stats::sd(modeldata[[v]], na.rm = TRUE)
                        m <- mean(modeldata[[v]], na.rm = TRUE)
                        predictors[[v]] <- c(m - s, m, m + s)
                    } else if (identical(predictors[[v]], "fivenum")) {
                        predictors[[v]] <- stats::fivenum
                    } else if (is.character(predictors[[v]])) {
                        msg <- '%s is a numeric variable. The summary shortcuts supported by the variables argument are: "iqr", "minmax", "sd", "2sd", "threenum", "fivenum".'
                        msg <- sprintf(msg, v)
                        insight::format_error(msg)
                    }

                    if (is.function(predictors[[v]])) {
                        tmp <- hush(predictors[[v]](modeldata[[v]]))
                        if (!is.numeric(tmp)) {
                            msg <- "The function supplied to the `variables` argument must return a numeric vector when applied to the %s variable."
                            msg <- sprintf(msg, v)
                            insight::format_error(msg)
                        } else {
                            predictors[[v]] <- tmp
                        }
                    }
                }

            } else if (isTRUE(variable_class[[v]] %in% c("factor", "character"))) {

                if (calling_function == "comparisons") {
                    valid <- c("reference", "sequential", "pairwise", "all")
                    flag1 <- checkmate::check_choice(predictors[[v]], choices = valid)
                    flag2 <- checkmate::check_vector(predictors[[v]], len = 2)
                    if (!isTRUE(flag1) && !isTRUE(flag2)) {
                        msg <- "The %s element of the `variables` argument must be a vector of length 2 or one of: %s"
                        msg <- sprintf(msg, v, paste(valid, collapse = ", "))
                        insight::format_error(msg)
                    }

                } else if (calling_function == "predictions") {

                    if (is.function(predictors[[v]])) {
                        tmp <- hush(predictors[[v]](modeldata[[v]]))
                        if (length(tmp) > 5) {
                            msg <- "The function supplied to `variables` returned more than 5 values for the %s variable. This can be dangerous because the full dataset will be duplicated many times. If this is truly what you want, you can supply the actual desired values explicitly to the `variables` argument instead of a function."
                            msg <- sprintf(msg, v)
                            insight::format_error(msg)
                        }
                    }

                    if (is.character(predictors[[v]]) || is.factor(predictors[[v]])) {
                        if (!all(as.character(predictors[[v]]) %in% as.character(modeldata[[v]]))) {
                            invalid <- intersect(
                                as.character(predictors[[v]]),
                                c("pairwise", "reference", "sequential", "revpairwise", "revreference", "revsequential"))
                            if (length(invalid) > 0) {
                                msg <- "These values are only supported by the `variables` argument in the `comparisons()` function: %s"
                                msg <- sprintf(msg, paste(invalid, collapse = ", "))
                            } else {
                                msg <- "Some elements of the `variables` argument are not in their original data. Check this variable: %s"
                                msg <- sprintf(msg, v)
                            }
                            insight::format_error(msg)
                        }
                    }
                }
            }
        }
    }

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

    }

    for (v in names(predictors)) {
        if (isTRUE(variable_class[[v]] == "numeric")) {
            fun <- fun_numeric
            lab <- lab_numeric
        } else {
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
