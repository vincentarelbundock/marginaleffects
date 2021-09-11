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


sanity_return_data <- function(return_data) {
    checkmate::assert_flag(return_data)
    return(return_data)
}

sanity_model <- function(model) {
    supported <- list("betareg",
                      "bife",
                      "clm",
                      "fixest",
                      c("Gam", "glm", "lm"),
                      "glm",
                      "glmerMod",
                      "hurdle",
                      "ivreg",
                      "lm",
                      "lmerMod",
                      "lm_robust",
                      "loess",
                      c("lrm", "rms", "glm"),
                      # "multinom",
                      "polr",
                      "speedglm",
                      "speedlm",
                      c("tobit", "survreg"))
    flag <- FALSE
    for (sup in supported) {
        if (all(sup %in% class(model))) {
            flag <- TRUE
        }
    }
    if (isFALSE(flag)) {
        msg <- "Models of class %s are not supported. Supported model classes include: %s."
        msg <- sprintf(msg, class(model)[1], paste(supported, collapse = ", "))
        stop(msg)
    }
    return(model)
}


sanity_group_names <- function (model) {
    group_models <- c("multinom", "polr")
    if (any(group_models %in% class(model))) {
        group_names <- levels(insight::get_response(model))
    } else {
        group_names <- "main"
    }
    return(group_names)
}


sanity_newdata <- function(model, newdata) {
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


sanity_variables <- function(model, newdata, variables) {
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)

    # guess variables
    if (is.null(variables)) {

        known <- c("dydx", "conditional", "zero_inflated")
        variables <- insight::find_variables(model)
        variables <- variables[names(variables) %in% known]
        variables <- unique(unlist(variables))
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


sanity_vcov <- function(model, vcov) {

    # lme4 produces a distinct matrix type
    if (inherits(vcov, "dpoMatrix")) {
            vcov <- as.matrix(vcov)
    }

    # assert affer dpoMatrix conversion
    checkmate::assert(
        checkmate::check_flag(vcov),
        checkmate::check_matrix(vcov, col.names = "unique", row.names = "unique", null.ok = TRUE))

    # skip
    if (isFALSE(vcov)) {
        vcov <- FALSE
    }

    # unsupported models
    unsupported <- c("clm", "loess", "polr")
    if (!isFALSE(vcov) && any(unsupported %in% class(model))) {
        vcov <- NULL
        warning(sprintf("Variance estimates are not yet supported for objects of class %s. Set `vcov=NULL` to silence this warning.", class(model))[1])
    }

    # TRUE: try to extract a vcov (TODO: implement get_vcov)
    if (isTRUE(vcov)) {
        vcov <- try(get_vcov(model), silent = TRUE)
        if (inherits(vcov, "try-error")) {
            vcov <- NULL
            warning(sprintf('Unable to extract a variance-covariance matrix from model of class "%s" using the `stats::vcov` function. The `vcov` argument was switched to `FALSE`. Please supply a named matrix to produce uncertainty estimates.', class(model)[1]))
            # dpoMatrix conversion
        }
        vcov <- as.matrix(vcov)
    } 


    # TODO: Test if the names of the matrix match the names of the coefficients.
    # This could be dangerous, so leaving as a Github issue until I have time for serious work.
    if (isFALSE(vcov)) {
        vcov <- NULL
    }

    return(vcov)
}


sanity_prediction_type <- function(model, prediction_type) {
    checkmate::assert_character(prediction_type, min.len = 1, null.ok = FALSE)

    if ("clm" %in% class(model)) {
        prediction_type[prediction_type == "probs"] <- "prob"
        idx <- !prediction_type %in% c("prob", "class", "cum.prob", "linear_predictor")
        prediction_type[idx] <- "prob"
        prediction_type <- unique(prediction_type)
        if (any(idx)) {
            warning(sprintf('The only `prediction_type` supported for models of class `%s` are "prob", "cum.prob", and "linear.predictor".', class(model)[1]))
        }
    }

    if (any(c("multinom", "nnet") %in% class(model))) {
        if (any(prediction_type != "probs")) {
            warning(sprintf('The only `prediction_type` supported for models of class `%s` is `"probs"`. The value of the argument was adjusted automatically. Modify the argument manually to silence this warning.', class(model)[1]))
            prediction_type <- "probs"
        }
    }

    return(prediction_type)
}
