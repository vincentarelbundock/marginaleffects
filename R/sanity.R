#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
  flag <- requireNamespace(library_name, quietly = TRUE)
  if (isFALSE(flag)) {
      msg <- sprintf("Please install the `%s` package.", library_name)
      return(msg)
  } else {
      return(TRUE)
  }
}
assert_dependency <- checkmate::makeAssertionFunction(check_dependency)


sanity_dydx_return_data <- function(return_data) {
    checkmate::assert_flag(return_data)
    return(return_data)
}


sanity_dydx_model <- function(model) {
    supported <- c("betareg",
                   "clm",
                   "fixest",
                   "glm",
                   "glmerMod",
                   "ivreg",
                   "lm",
                   "lmerMod",
                   "loess",
                   # "multinom",
                   "polr")
    if (!any(supported %in% class(model))) {
        msg <- "Models of class %s are not supported. Supported model classes include: %s."
        msg <- sprintf(msg, class(model)[1], paste(supported, collapse = ", "))
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
    if ("group" %in% colnames(newdata)) {
        stop('The string "group" cannot be a column name in `newdata`. It is used in the generic, standardized, and tidy output of the `marginaleffects` function. Sharing a column name could cause confusion. Please use a more descriptive variable name in your dataset.')
    }
    return(newdata)
}


sanity_dydx_variables <- function(model, newdata, variables) {
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)

    # guess variables
    if (is.null(variables)) {
        variables <- insight::find_variables(model)$conditional
    }

    # dydx variables
    idx <- sapply(newdata[, variables, drop = FALSE], is.numeric)
    variables_dydx <- variables[idx]

    # contrast
    idx <- sapply(newdata[, variables, drop = FALSE], 
                  function(x) is.logical(x) | is.factor(x) | is.character(x))
    variables_contrast <- variables[idx]

    # others
    variables_others <- setdiff(variables, c(variables_dydx, variables_contrast))

    # output
    out <- list("dydx" = variables_dydx, 
                "contrast" = variables_contrast,
                "others" = variables_others)
    return(out)
}


sanity_dydx_variance <- function(model, variance) {

    # lme4 produces a distinct matrix type
    if (inherits(variance, "dpoMatrix")) {
            variance <- as.matrix(variance)
    }

    # assert affer dpoMatrix conversion
    checkmate::assert(
        checkmate::check_flag(variance),
        checkmate::check_matrix(variance, col.names = "unique", row.names = "unique", null.ok = TRUE))

    # skip
    if (isFALSE(variance)) {
        variance <- FALSE
    }

    # unsupported models
    unsupported <- c("clm", "loess", "polr")
    if (!isFALSE(variance) && any(unsupported %in% class(model))) {
        variance <- NULL
        warning(sprintf("Variance estimates are not yet supported for objects of class %s. Set `variance=NULL` to silence this warning.", class(model))[1])
    }

    # TRUE: try to extract a vcov (TODO: implement get_vcov)
    if (isTRUE(variance)) {
        variance <- try(stats::vcov(model), silent = TRUE)
        if (inherits(variance, "try-error")) {
            variance <- NULL
            warning(sprintf('Unable to extract a variance-covariance matrix from model of class "%s" using the `stats::vcov` function. The `variance` argument was switched to `FALSE`. Please supply a named matrix to produce uncertainty estimates.', class(model)[1]))
            # dpoMatrix conversion
        }
        variance <- as.matrix(variance)
    } 


    # TODO: Test if the names of the matrix match the names of the coefficients.
    # This could be dangerous, so leaving as a Github issue until I have time for serious work.
    if (isFALSE(variance)) {
        variance <- NULL
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
