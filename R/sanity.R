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


## unused for now
# sanity_weights <- function(mfx, weights) {
#     checkmate::assert_numeric(weights, null.ok = TRUE, len = length(unique(mfx$rowid)))
#     return(weights)
# }


sanity_return_data <- function(return_data) {
    checkmate::assert_flag(return_data)
    return(return_data)
}



sanity_newdata <- function(model, newdata) {
    checkmate::check_data_frame(newdata, 
                                null.ok = TRUE, 
                                any.missing = FALSE)
    if (is.null(newdata)) {
        newdata <- insight::get_data(model)
    }
    # if ("group" %in% colnames(newdata)) {
    #     stop('The string "group" cannot be a column name in `newdata`. It is used in the generic, standardized, and tidy output of the `marginaleffects` function. Sharing a column name could cause confusion. Please use a more descriptive variable name in your dataset.')
    # }
    return(newdata)
}

sanity_variables <- function(model, newdata, variables) {
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)
    checkmate::assert_data_frame(newdata, min.row = 1, null.ok = TRUE)

    if (!is.null(model)) {
        origindata <- insight::get_data(model)
    } else {
        origindata <- newdata
    }

    if (is.null(newdata)) {
        newdata <- origindata
    }

    # get variables
    if (is.null(variables)) {
        variables_list <- insight::find_variables(model)
        variables_list[["response"]] <- NULL
    } else {
        variables_list <- list("conditional" = variables)
    }
    variables <- unique(unlist(variables_list))

    # check missing character levels
    # Character variables are treated as factors by model-fitting functions,
    # but unlike factors, they do not not keep a record of all factor levels.
    # This poses problem when feeding `newdata` to `predict`, which often
    # breaks (via `model.matrix`) when the data does not include all possible
    # factor levels.
    levels_character <- list()
    for (v in variables) {
        if (v %in% colnames(origindata)) {
            if (is.character(origindata[[v]])) {
                levels_character[[v]] <- unique(origindata[[v]])
            }
        }
    }
    attr(variables_list, "levels_character") <- levels_character

    # check missing variables
    miss <- setdiff(variables, colnames(newdata))
    if (length(miss) > 0) {
        stop(sprintf("Variables missing from `newdata` and/or the data extracted from the model objects: %s", 
                     paste(miss, collapse = ", ")))
    }

    return(variables_list)
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


sanity_predict_vector <- function(pred, model, newdata, type) {
    if (!isTRUE(checkmate::check_atomic_vector(pred))) {
        msg <- sprintf(
'`predict(model, type = "%s")` was called on a model of class `%s`, but this command did not produce the expected outcome: A numeric vector of length %s. This can sometimes happen when users try compute a marginal effect for some models with grouped or multivariate outcome which are not supported yet by `marginaleffects` package. Please consult your modeling package documentation to learn what alternative `type` arguments are accepted by the `predict` method, or file a feature request on Github:  https://github.com/vincentarelbundock/marginaleffects/issues',
        type, class(model)[1], nrow(newdata))
        stop(msg)
    }
}


sanity_predict_numeric <- function(pred, model, newdata, type) {
    if (!isTRUE(checkmate::check_numeric(pred))) {
        msg <- sprintf(
'`predict(model, type = "%s")` was called on a model of class `%s`, but this command did not produce the expected outcome: A numeric vector of length %s. This can sometimes happen when users try compute a marginal effect for an outcome type which is unsupported, or which cannot be differentiated. Please consult your modeling package documentation to learn what alternative `type` arguments are accepted by the `predict` method.',
        type, class(model)[1], nrow(newdata))
        stop(msg)
    }
}
