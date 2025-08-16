# Helper functions for add_variables()

get_predictors <- function(variables, mfx) {
    model <- mfx@model
    modeldata <- mfx@modeldata
    newdata <- mfx@newdata
    calling_function <- mfx@calling_function

    stop_zero <- function(predictors) {
        if (length(predictors) == 0) {
            msg <- "There is no valid predictor variable. Please change the `variables` argument or supply an alternative data frame to the `newdata` argument."
            stop_sprintf(msg)
        }
    }

    # variables -> predictors (named list)
    if (isTRUE(checkmate::check_list(variables, names = "unique"))) {
        predictors <- variables
    } else if (isTRUE(checkmate::check_character(variables))) {
        predictors <- stats::setNames(
            rep(list(NULL), length(variables)),
            variables)
    } else {
        # we don't want group ids in multi-level models and other weirdness
        predictors <- insight::find_predictors(mfx@model, component = "all")

        # mhurdle names variables weirdly
        if (inherits(model, "mhurdle")) {
            predictors <- unlist(predictors)
            predictors <- list(conditional = predictors)
        }

        known <- c("fixed", "conditional", "zero_inflated", "scale", "nonlinear")
        if (any(known %in% names(predictors))) {
            predictors <- predictors[known]
        }
        predictors <- unlist(predictors)
        predictors <- setdiff(predictors, mfx@variable_names_response)
        predictors <- stats::setNames(
            rep(list(NULL), length(predictors)),
            predictors)
    }
    stop_zero(predictors)

    # warn if missing from `newdata`
    miss <- setdiff(names(predictors), colnames(newdata))
    if (length(miss) > 0) {
        msg <- sprintf("Variable(s) not found in `newdata`: %s.", toString(miss))
        warn_sprintf(msg)
        predictors <- predictors[names(predictors) %in% colnames(newdata)]
    }
    stop_zero(predictors)

    # warn if matrix column
    mat <- sapply(names(predictors), function(v) {
        check_variable_class(mfx, variable = v, compare = "matrix")
    })
    if (any(mat)) {
        warn_sprintf("Matrix columns are not supported: %s. Use the `variables` argument to specify valid predictors, or use a function like `drop()` to convert your matrix columns into vectors.", toString(names(mat)[mat]))
        predictors <- predictors[setdiff(names(predictors), names(mat)[mat])]
    }
    stop_zero(predictors)

    # sometimes `insight` returns interaction component as if
    # it were a constituent variable
    idx <- grepl(":", names(predictors))
    predictors <- predictors[!idx]
    stop_zero(predictors)

    return(predictors)
}

sanitize_predictor_container <- function(predictors) {
    # character -> list
    if (isTRUE(checkmate::check_character(predictors))) {
        predictors <- stats::setNames(
            rep(list(NULL), length(predictors)),
            predictors
        )
    }
    predictors
}

add_prediction_functions <- function(predictors, modeldata) {
    # functions to values
    # only for predictions; get_comparisons_data_numeric handles this for comparisons()
    # do this before NULL-to-defaults so we can fill it in with default in case of failure
    for (v in names(predictors)) {
        if (is.function(predictors[[v]])) {
            tmp <- hush(predictors[[v]](modeldata[[v]]))
            if (is.null(tmp)) {
                msg <- sprintf(
                    "The `%s` function produced invalid output when applied to the dataset used to fit the model.",
                    v
                )
                warn_sprintf(msg)
            }
            predictors[[v]] <- hush(predictors[[v]](modeldata[[v]]))
        }
    }
    predictors
}

add_numeric_shortcuts <- function(predictors, mfx) {
    # string shortcuts for predictions only
    modeldata <- mfx@modeldata
    for (v in names(predictors)) {
        if (check_variable_class(mfx, v, "numeric")) {
            if (identical(predictors[[v]], "iqr")) {
                predictors[[v]] <- stats::quantile(
                    modeldata[[v]],
                    probs = c(0.25, 0.75),
                    na.rm = TRUE
                )
            } else if (identical(predictors[[v]], "minmax")) {
                predictors[[v]] <- c(
                    min(modeldata[[v]], na.rm = TRUE),
                    max(modeldata[[v]], na.rm = TRUE)
                )
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
                predictors[[v]] <- stats::fivenum(modeldata[[v]])
            } else if (is.character(predictors[[v]])) {
                msg <- sprintf(
                    '%s is a numeric variable. The summary shortcuts supported by the variables argument are: "iqr", "minmax", "sd", "2sd", "threenum", "fivenum".',
                    v
                )
                stop_sprintf(msg)
            }
        }
    }
    predictors
}

add_default_values <- function(predictors, mfx) {
    calling_function <- mfx@calling_function

    # NULL to defaults
    for (v in names(predictors)) {
        if (is.null(predictors[[v]])) {
            if (check_variable_class(mfx, v, "binary")) {
                predictors[[v]] <- 0:1
            } else if (check_variable_class(mfx, v, "numeric")) {
                if (calling_function == "comparisons") {
                    predictors[[v]] <- 1
                } else if (calling_function == "predictions") {
                    v_unique <- unique(mfx@modeldata[[v]])
                    if (length(v_unique) < 6) {
                        predictors[[v]] <- v_unique
                    } else {
                        predictors[[v]] <- stats::fivenum(mfx@modeldata[[v]])
                    }
                }
            } else {
                if (calling_function == "comparisons") {
                    predictors[[v]] <- "reference"
                } else if (calling_function == "predictions") {
                    # TODO: warning when this is too large. Here or elsewhere?
                    predictors[[v]] <- unique(mfx@modeldata[[v]])
                }
            }
        }
    }
    predictors
}

sanitize_predictor_specs <- function(predictors, mfx) {
    modeldata <- mfx@modeldata
    newdata <- mfx@newdata
    calling_function <- mfx@calling_function

    # shortcuts and validity
    for (v in names(predictors)) {
        if (
            isTRUE(checkmate::check_data_frame(
                predictors[[v]],
                nrows = nrow(newdata)
            ))
        ) {
            # do nothing, but don't take the other validity check branches
        } else if (check_variable_class(mfx, v, "binary")) {
            if (
                !isTRUE(checkmate::check_numeric(predictors[[v]])) ||
                    !is_binary(predictors[[v]])
            ) {
                msg <- sprintf(
                    "The `%s` variable is binary. The corresponding entry in the `variables` argument must be 0 or 1.",
                    v
                )
                stop_sprintf(msg)
            }
            # get_comparisons_data requires both levels
            if (calling_function == "comparisons") {
                if (length(predictors[[v]]) != 2) {
                    msg <- sprintf(
                        "The `%s` variable is binary. The corresponding entry in the `variables` argument must be a vector of length 2.",
                        v
                    )
                    stop_sprintf(msg)
                }
            }
        } else if (check_variable_class(mfx, v, "numeric")) {
            if (calling_function == "comparisons") {
                # For comparisons(), the string shortcuts are processed in contrast_data_* functions because we need fancy labels.
                # Eventually it would be nice to consolidate, but that's a lot of work.
                valid_str <- c("iqr", "minmax", "sd", "2sd")
                flag <- isTRUE(checkmate::check_numeric(
                    predictors[[v]],
                    min.len = 1,
                    max.len = 2
                )) ||
                    isTRUE(checkmate::check_choice(
                        predictors[[v]],
                        choices = valid_str
                    )) ||
                    isTRUE(checkmate::check_function(predictors[[v]]))
                if (!isTRUE(flag)) {
                    msg <- "The %s element of the `variables` argument is invalid."
                    msg <- sprintf(msg, v)
                    stop_sprintf(msg)
                }
            } else if (calling_function == "predictions") {
                # string shortcuts
                if (identical(predictors[[v]], "iqr")) {
                    predictors[[v]] <- stats::quantile(
                        modeldata[[v]],
                        probs = c(0.25, 0.75),
                        na.rm = TRUE
                    )
                } else if (identical(predictors[[v]], "minmax")) {
                    predictors[[v]] <- c(
                        min(modeldata[[v]], na.rm = TRUE),
                        max(modeldata[[v]], na.rm = TRUE)
                    )
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
                    msg <- sprintf(
                        '%s is a numeric variable. The summary shortcuts supported by the variables argument are: "iqr", "minmax", "sd", "2sd", "threenum", "fivenum".',
                        v
                    )
                    stop_sprintf(msg)
                }
            }
        } else {
            if (calling_function == "comparisons") {
                valid <- c(
                    "reference",
                    "sequential",
                    "pairwise",
                    "all",
                    "revpairwise",
                    "revsequential",
                    "revreference"
                )
                # minmax needs an actual factor in the original data to guarantee correct order of levels.
                if (is.factor(modeldata[[v]])) {
                    valid <- c(valid, "minmax")
                }
                flag1 <- checkmate::check_choice(predictors[[v]], choices = valid)
                flag2 <- checkmate::check_vector(predictors[[v]], len = 2)
                flag3 <- checkmate::check_data_frame(
                    predictors[[v]],
                    nrows = nrow(newdata),
                    ncols = 2
                )
                flag4 <- checkmate::check_function(predictors[[v]])
                flag5 <- checkmate::check_data_frame(predictors[[v]])
                if (
                    !isTRUE(flag1) &&
                        !isTRUE(flag2) &&
                        !isTRUE(flag3) &&
                        !isTRUE(flag4) &&
                        !isTRUE(flag5)
                ) {
                    msg <- "The %s element of the `variables` argument must be a vector of length 2 or one of: %s"
                    msg <- sprintf(msg, v, toString(valid))
                    stop_sprintf(msg)
                }
            } else if (calling_function == "predictions") {
                if (is.character(predictors[[v]]) || is.factor(predictors[[v]])) {
                    if (
                        !all(
                            as.character(predictors[[v]]) %in% as.character(modeldata[[v]])
                        )
                    ) {
                        invalid <- intersect(
                            as.character(predictors[[v]]),
                            c(
                                "pairwise",
                                "reference",
                                "sequential",
                                "revpairwise",
                                "revreference",
                                "revsequential"
                            )
                        )
                        if (length(invalid) > 0) {
                            msg <- "These values are only supported by the `variables` argument in the `comparisons()` function: %s"
                            msg <- sprintf(msg, toString(invalid))
                        } else {
                            msg <- "Some elements of the `variables` argument are not in their original data. Check this variable: %s"
                            msg <- sprintf(msg, v)
                        }
                        stop_sprintf(msg)
                    }
                }
            }
        }
    }
    predictors
}

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
            check_variable_class(mfx, v, "numeric") &&
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

get_weight_variables <- function(mfx) {
    w <- mfx@variable_names_wts
    w <- intersect(w, colnames(mfx@newdata))
    w
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


detect_variable_class <- function(modeldata, model = NULL) {
    if (is.null(modeldata)) {
        return(character())
    }

    # this can be costly on large datasets, when only a portion of
    # variables are used in the model
    variables <- NULL
    if (is.null(model)) {
        variables <- tryCatch(
            unlist(insight::find_variables(model, flatten = TRUE, verbose = FALSE), use.names = FALSE),
            error = function(e) NULL
        )
    }
    if (is.null(variables)) variables <- colnames(modeldata)

    out <- modeldata

    cl <- NULL
    for (col in variables) {
        if (is.matrix(out[[col]])) {
            cl[col] <- "matrix"
        } else if (is.logical(out[[col]])) {
            cl[col] <- "logical"
        } else if (is.character(out[[col]])) {
            cl[col] <- "character"
        } else if (is.factor(out[[col]])) {
            cl[col] <- "factor"
        } else if (inherits(out[[col]], "Surv")) {
            # is numeric but breaks the %in% 0:1 check
            cl[col] <- "other"
        } else if (is.numeric(out[[col]])) {
            if (is_binary(out[[col]])) {
                cl[col] <- "binary"
            } else {
                cl[col] <- "numeric"
            }
        } else {
            cl[col] <- "other"
        }
    }

    if (is.null(model)) {
        return(cl)
    }

    te <- hush(insight::find_terms(model, flatten = TRUE))

    # in-formula factor
    regex <- "^(^as\\.factor|^factor)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "factor"

    # in-formula categoricals
    regex <- "^(^mo|^strata)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "strata"

    # in-formula numeric
    regex <- "^numeric\\((.*)\\)$|^as.numeric\\((.*)\\)$"
    idx <- gsub(
        regex,
        "\\1",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "numeric"

    # in-formula logical
    regex <- "^logical\\((.*)\\)$|^as.logical\\((.*)\\)$"
    idx <- gsub(
        regex,
        "\\1",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "logical"

    # in-formula: fixest::i()
    fi <- NULL
    idx <- grepl("^i\\(.*\\)$", te)
    if (sum(idx) > 0) {
        arg1 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[2]])))
        arg2 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[3]])))
        arg1 <- unlist(arg1, recursive = TRUE)
        arg2 <- unlist(arg2, recursive = TRUE)
        arg2 <- gsub("^i\\.", "", arg2[grepl("^i\\.", arg2)])
        fi <- unique(c(arg1, arg2))
    }
    for (f in fi) {
        cl[f] <- "cluster"
    }

    return(cl)
}


check_variable_class <- function(newdata, variable = NULL, compare = NULL) {
    if (inherits(newdata, "marginaleffects_internal")) {
        cl <- newdata@variable_class
    } else if ("marginaleffects_variable_class" %in% names(attributes(newdata))) {
        cl <- attributes(newdata)$marginaleffects_variable_class
    } else if (isTRUE(checkmate::check_character(newdata))) {
        cl <- newdata
    } else {
        cl <- character()
    }

    if (is.null(compare) && is.null(variable)) {
        out <- cl
    } else if (is.null(compare)) {
        out <- if (variable %in% names(cl)) cl[variable] else NA
    } else if (is.null(variable)) {
        if (isTRUE(compare == "categorical")) {
            out <- cl[
                cl %in%
                    c("factor", "character", "logical", "strata", "cluster", "binary")
            ]
        } else {
            out <- cl[cl %in% compare]
        }
    } else {
        if (variable %in% names(cl)) {
            if (isTRUE(compare == "categorical")) {
                out <- cl[variable] %in%
                    c("factor", "character", "logical", "strata", "cluster", "binary")
            } else {
                out <- cl[variable] %in% compare
            }
        } else {
            out <- FALSE
        }
    }

    return(out)
}
