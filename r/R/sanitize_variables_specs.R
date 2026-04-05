# Predictor specification validation, defaults, and shortcuts

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
        if (check_variable_class(mfx, v, "numeric") || check_variable_class(mfx, v, "integer")) {
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
            } else if (check_variable_class(mfx, v, "numeric") || check_variable_class(mfx, v, "integer")) {
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
                    if (is.factor(mfx@modeldata[[v]]) && !is.null(levels(mfx@modeldata[[v]]))) {
                        predictors[[v]] <- as.factor(levels(mfx@modeldata[[v]]))
                    } else {
                        predictors[[v]] <- unique(mfx@modeldata[[v]])
                    }
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
        } else if (check_variable_class(mfx, v, "numeric") || check_variable_class(mfx, v, "integer")) {
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
                    valid_values <- as.character(modeldata[[v]])
                    if (is.factor(modeldata[[v]])) {
                        valid_values <- union(valid_values, levels(modeldata[[v]]))
                    }
                    if (
                        !all(
                            as.character(predictors[[v]]) %in% valid_values
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
