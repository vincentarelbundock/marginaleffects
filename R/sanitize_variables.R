# input: character vector or named list
# output: named list of lists where each element represents a variable with: name, value, function, label
sanitize_variables <- function(variables,
                               model,
                               newdata, # need for NumPyro where `find_variables()`` does not work
                               modeldata,
                               comparison = NULL,
                               by = NULL,
                               cross = FALSE,
                               calling_function = "comparisons",
                               eps = NULL) {
  checkmate::assert(
    checkmate::check_null(variables),
    checkmate::check_character(variables, min.len = 1, names = "unnamed"),
    checkmate::check_list(variables, min.len = 1, names = "unique"),
    combine = "or")

  # extensions with no `get_data()`
  if (is.null(modeldata) || nrow(modeldata) == 0) {
    modeldata <- set_variable_class(newdata, model)
    no_modeldata <- TRUE
  } else {
    no_modeldata <- FALSE
  }

  # variables is NULL: get all variable names from the model
  if (is.null(variables)) {
    # mhurdle names the variables weirdly
    if (inherits(model, "mhurdle")) {
      predictors <- insight::find_predictors(model, flatten = TRUE, verbose = FALSE)
      predictors <- list(conditional = predictors)
    } else {
      predictors <- insight::find_variables(model)
    }

    # unsupported models like pytorch
    if (length(predictors) == 0 || (length(predictors) == 1 && names(predictors) == "response")) {
      dv <- hush(unlist(insight::find_response(model, combine = FALSE), use.names = FALSE))
      predictors <- setdiff(hush(colnames(newdata)), c(dv, "rowid"))
    } else {
      known <- c("fixed", "conditional", "zero_inflated", "scale", "nonlinear")
      if (any(known %in% names(predictors))) {
        predictors <- predictors[known]
        # sometimes triggered by multivariate brms models where we get nested
        # list: predictors$gear$hp
      } else {
        predictors <- unlist(predictors, recursive = TRUE, use.names = FALSE)
        predictors <- unique(predictors)
      }
      # flatten
      predictors <- unique(unlist(predictors, recursive = TRUE, use.names = FALSE))
    }
  } else {
    predictors <- variables
  }

  # character -> list
  if (isTRUE(checkmate::check_character(predictors))) {
    predictors <- stats::setNames(rep(list(NULL), length(predictors)), predictors)
  }

  # reserved keywords
  # Issue #697: we used to allow "group", as long as it wasn't in
  # `variables`, but this created problems with automatic `by=TRUE`. Perhaps
  # I could loosen this, but there are many interactions, and the lazy way is
  # just to forbid entirely.
  reserved <- c(
    "rowid", "group", "term", "contrast", "estimate",
    "std.error", "statistic", "conf.low", "conf.high", "p.value",
    "p.value.nonsup", "p.value.noninf", "by")
  # if no modeldata is available, we use `newdata`, but that often has a
  # `rowid` column. This used to break the extensions.Rmd vignette.
  if (no_modeldata) {
    reserved <- setdiff(reserved, "rowid")
  }
  bad <- unique(intersect(c(names(predictors), colnames(modeldata)), reserved))
  if (length(bad) > 0) {
    msg <- c(
      "These variable names are forbidden to avoid conflicts with the outputs of `marginaleffects`:",
      sprintf("%s", paste(sprintf('"%s"', bad), collapse = ", ")),
      "Please rename your variables before fitting the model.")
    insight::format_error(msg)
  }

  # when comparisons() only inludes one focal predictor, we don't need to specify it in `newdata`
  # when `variables` is numeric, we still need to include it, because in
  # non-linear model the contrast depend on the starting value of the focal
  # variable.
  found <- colnames(newdata)
  if (calling_function == "comparisons") {
    v <- NULL
    if (isTRUE(checkmate::check_string(variables))) {
      v <- variables
    } else if (isTRUE(checkmate::check_list(variables, len = 1, names = "named"))) {
      v <- names(variables)[1]
    }
    flag <- get_variable_class(modeldata, variable = v, compare = "categorical")
    if (!is.null(v) && isTRUE(flag)) {
      found <- c(found, v)
    }
  }


  # matrix predictors
  mc <- attr(newdata, "matrix_columns")
  if (length(mc) > 0 && any(names(predictors) %in% mc)) {
    predictors <- predictors[!names(predictors) %in% mc]
    insight::format_warning("Matrix columns are not supported. Use the `variables` argument to specify valid predictors, or use a function like `drop()` to convert your matrix columns into vectors.")
  }

  # missing variables
  miss <- setdiff(names(predictors), found)
  predictors <- predictors[!names(predictors) %in% miss]
  if (length(miss) > 0) {
    msg <- sprintf(
      "These variables were not found: %s.  Try specifying the `newdata` argument explicitly and make sure the missing variable is included.",
      paste(miss, collapse = ", "))
    insight::format_warning(msg)
  }

  # sometimes `insight` returns interaction component as if it were a constituent variable
  idx <- !grepl(":", names(predictors))
  predictors <- predictors[idx]

  # anything left?
  if (length(predictors) == 0) {
    msg <- "There is no valid predictor variable. Please change the `variables` argument or supply a new data frame to the `newdata` argument."
    insight::format_error(msg)
  }

  # functions to values
  # only for predictions; get_contrast_data_numeric handles this for comparisons()
  # do this before NULL-to-defaults so we can fill it in with default in case of failure
  if (calling_function == "predictions") {
    for (v in names(predictors)) {
      if (is.function(predictors[[v]])) {
        tmp <- hush(predictors[[v]](modeldata[[v]]))
        if (is.null(tmp)) {
          msg <- sprintf("The `%s` function produced invalid output when applied to the dataset used to fit the model.", v)
          insight::format_warning(msg)
        }
        predictors[[v]] <- hush(predictors[[v]](modeldata[[v]]))
      }
    }
  }

  # NULL to defaults
  for (v in names(predictors)) {
    if (is.null(predictors[[v]])) {
      if (get_variable_class(modeldata, v, "binary")) {
        predictors[[v]] <- 0:1
      } else if (get_variable_class(modeldata, v, "numeric")) {
        if (calling_function == "comparisons") {
          predictors[[v]] <- 1
        } else if (calling_function == "predictions") {
          v_unique <- unique(modeldata[[v]])
          if (length(v_unique) < 6) {
            predictors[[v]] <- v_unique
          } else {
            predictors[[v]] <- stats::fivenum(modeldata[[v]])
          }
        }
      } else {
        if (calling_function == "comparisons") {
          predictors[[v]] <- "reference"
        } else if (calling_function == "predictions") {
          # TODO: warning when this is too large. Here or elsewhere?
          predictors[[v]] <- unique(modeldata[[v]])
        }
      }
    }
  }

  # shortcuts and validity
  for (v in names(predictors)) {
    if (isTRUE(checkmate::check_data_frame(predictors[[v]], nrows = nrow(newdata)))) {
      # do nothing, but don't take the other validity check branches
    } else if (get_variable_class(modeldata, v, "binary")) {
      if (!isTRUE(checkmate::check_numeric(predictors[[v]])) || !is_binary(predictors[[v]])) {
        msg <- sprintf("The `%s` variable is binary. The corresponding entry in the `variables` argument must be 0 or 1.", v)
        insight::format_error(msg)
      }
      # get_contrast_data requires both levels
      if (calling_function == "comparisons") {
        if (length(predictors[[v]]) != 2) {
          msg <- sprintf("The `%s` variable is binary. The corresponding entry in the `variables` argument must be a vector of length 2.", v)
          insight::format_error(msg)
        }
      }
    } else if (get_variable_class(modeldata, v, "numeric")) {
      if (calling_function == "comparisons") {
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
          predictors[[v]] <- stats::quantile(modeldata[[v]], probs = c(0.25, 0.75), na.rm = TRUE)
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
          msg <- sprintf('%s is a numeric variable. The summary shortcuts supported by the variables argument are: "iqr", "minmax", "sd", "2sd", "threenum", "fivenum".', v)
          insight::format_error(msg)
        }
      }
    } else {
      if (calling_function == "comparisons") {
        valid <- c("reference", "sequential", "pairwise", "all", "revpairwise", "revsequential", "revreference")
        # minmax needs an actual factor in the original data to guarantee correct order of levels.
        if (is.factor(modeldata[[v]])) {
          valid <- c(valid, "minmax")
        }
        flag1 <- checkmate::check_choice(predictors[[v]], choices = valid)
        flag2 <- checkmate::check_vector(predictors[[v]], len = 2)
        flag3 <- checkmate::check_data_frame(predictors[[v]], nrows = nrow(newdata), ncols = 2)
        flag4 <- checkmate::check_function(predictors[[v]])
        flag5 <- checkmate::check_data_frame(predictors[[v]])
        if (!isTRUE(flag1) && !isTRUE(flag2) && !isTRUE(flag3) && !isTRUE(flag4) && !isTRUE(flag5)) {
          msg <- "The %s element of the `variables` argument must be a vector of length 2 or one of: %s"
          msg <- sprintf(msg, v, paste(valid, collapse = ", "))
          insight::format_error(msg)
        }
      } else if (calling_function == "predictions") {
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

  # sometimes weights don't get extracted by `find_variables()`
  w <- tryCatch(insight::find_weights(model), error = function(e) NULL)
  w <- intersect(w, colnames(newdata))
  others <- w


  # goals:
  # allow multiple function types: slopes() uses both difference and dydx
  # when comparison is defined, use that if it works or turn back to defaults
  # predictors list elements: name, value, function, label

  if (is.null(comparison)) {
    fun_numeric <- fun_categorical <- comparison_function_dict[["difference"]]
    lab_numeric <- lab_categorical <- comparison_label_dict[["difference"]]
  } else if (is.function(comparison)) {
    fun_numeric <- fun_categorical <- comparison
    lab_numeric <- lab_categorical <- "custom"
  } else if (is.character(comparison)) {
    # switch to the avg version when there is a `by` function
    if ((isTRUE(checkmate::check_character(by)) || isTRUE(by)) && !isTRUE(grepl("avg$", comparison))) {
      comparison <- paste0(comparison, "avg")
    }

    # weights if user requests `avg` or automatically switched
    if (isTRUE(grepl("avg$", comparison)) && "marginaleffects_wts_internal" %in% colnames(newdata)) {
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

  for (v in names(predictors)) {
    if (get_variable_class(modeldata, v, "numeric") && !get_variable_class(modeldata, v, "binary")) {
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
      "comparison" = comparison)
  }

  # epsilon for finite difference
  for (v in names(predictors)) {
    if (!is.null(eps)) {
      predictors[[v]][["eps"]] <- eps
    } else if (is.numeric(modeldata[[v]])) {
      predictors[[v]][["eps"]] <- 1e-4 * diff(range(modeldata[[v]], na.rm = TRUE, finite = TRUE))
      # 1-row grid has 0 range
      if (predictors[[v]][["eps"]] == 0) {
        predictors[[v]][["eps"]] <- 1e-4
      }
    } else {
      predictors[[v]]["eps"] <- list(NULL)
    }
  }

  # can't take the slope of an outcome, except in weird brms models (issue #1006)
  if (!inherits(model, "brmsfit") || !isTRUE(length(model$formula$forms) > 1)) {
    dv <- hush(unlist(insight::find_response(model, combine = FALSE), use.names = FALSE))
    # sometimes insight doesn't work
    if (length(dv) > 0) {
      predictors <- predictors[setdiff(names(predictors), dv)]
    }
  }
  if (length(predictors) == 0) {
    insight::format_error("There is no valid predictor variable. Please make sure your model includes predictors and use the `variables` argument.")
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

  # sort variables alphabetically
  predictors <- predictors[sort(names(predictors))]
  others <- others[sort(names(others))]

  # internal variables are not predictors
  predictors <- predictors[!names(predictors) %in% c("marginaleffects_wts_internal", "rowid_dedup")]
  if (length(predictors) == 0 && calling_function == "comparisons") {
    insight::format_error("No valid predictor variable.")
  }

  # output
  out <- list(conditional = predictors, others = others)

  return(out)
}
