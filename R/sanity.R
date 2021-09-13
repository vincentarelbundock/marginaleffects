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
                      c("geeglm", "gee", "glm"),
                      "glm",
                      "gls",
                      "glmerMod",
                      "hurdle",
                      "ivreg",
                      "lm",
                      "lmerMod",
                      "lm_robust",
                      "loess",
                      c("lrm", "lm"),
                      c("lrm", "rms", "glm"),
                      c("negbin", "glm", "lm"),
                      c("plm", "panelmodel"),
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

    # model-specific checks
    if (all(c("plm", "panelmodel") %in% class(model))) {
        if ("within" %in% model$args$model) {
            stop('The `plm::predict` function does not appear to support the `newdata` argument when `plm(model="within")`. Therefore, `marginaleffects` cannot support "within" models, even if it supports many other models produced by the `plm` package.')
        }
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
    checkmate::assert_data_frame(newdata, min.row = 1, null.ok = TRUE)

    # get data
    if (is.null(newdata)) {
        newdata <- insight::get_data(model)
    }

    # get variables
    if (is.null(variables)) {
        variables_list <- insight::find_variables(model)
        variables_list[["response"]] <- NULL
    } else {
        variables_list <- list("user" = variables)
    }
    variables <- unique(unlist(variables_list))

    # check missing variables
    miss <- setdiff(variables, colnames(newdata))
    if (length(miss) > 0) {
        stop(sprintf("Variables missing from `newdata` and/or the data extracted from the model objects: %s", 
                     paste(miss, collapse = ", ")))
    }


    # clusters (before dydx, in case cluster is numeric)
    if ("cluster" %in% names(variables_list)) {
        variables_cluster <- variables_list[["cluster"]]
        variables <- setdiff(variables, variables_cluster)
    } else {
        variables_cluster <- character()
    }

    # dydx variables
    if (length(variables) > 0) {
        idx <- sapply(variables, function(x) is.numeric(newdata[[x]]))
        variables_dydx <- variables[idx]
        variables <- setdiff(variables, variables_dydx)
    } else {
        variables_dydx <- character()
    }

    # contrast
    if (length(variables) > 0) {
        idx <- sapply(variables, function(x) 
                      is.logical(newdata[[x]]) | is.factor(newdata[[x]]) | is.character(newdata[[x]]))
        variables_contrast <- variables[idx]
        variables <- setdiff(variables, variables_contrast)
    } else {
        variables_contrast <- character()
    }

    # output
    out <- list("dydx" = variables_dydx, 
                "contrast" = variables_contrast,
                "cluster" = variables_cluster,
                "other" = variables)
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
