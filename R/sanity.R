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


sanitize_return_data <- function() {
    return_data <- getOption("marginaleffects_return_data", default = TRUE)
    checkmate::assert_flag(return_data)
    return(return_data)
}


sanitize_numDeriv_method <- function() {
    numDeriv_method <- getOption("marginaleffects_numDeriv_method", default = "simple")
    checkmate::assert_choice(numDeriv_method, choices = c("simple", "complex", "Richardson"))
    return(numDeriv_method)
}


sanity_newdata <- function(model, newdata) {
    checkmate::check_data_frame(newdata,
                                null.ok = TRUE,
                                any.missing = FALSE)
    if (is.null(newdata)) {
        newdata <- insight::get_data(model)
    }

    # if there are no categorical variables in `newdata`, check the model terms
    # to find transformation and warn accordingly.
    categorical_variables <- find_categorical(newdata = newdata, model = model)
    flag <- FALSE
    if (length(categorical_variables) == 0) {
        termlabs <- try(attr(stats::terms(model), "term.labels"), silent = TRUE)
        termlabs <- try(any(grepl("^factor\\(|^as.factor\\(|^as.logical\\(", termlabs)), silent = TRUE)
        if (isTRUE(termlabs)) {
            flag <- TRUE
        }
    }
    # This may be useful but it raises way too many warnings
    #} else {
    #     for (cv in categorical_variables) {
    #         if (is.numeric(newdata[[cv]])) {
    #             flag <- TRUE
    #         }
    #     }
    # }
    if (isTRUE(flag)) {
        warning("When using `marginaleffects`, it is safer to convert variables to factors or logicals in the dataset before fitting the model, rather than by wrapping terms in `factor()` or `as.logical() in the model formula.")
    }

    return(newdata)
}

sanity_variables <- function(model, newdata, variables) {
    checkmate::assert_character(variables, min.len = 1, null.ok = TRUE)
    checkmate::assert_data_frame(newdata, min.row = 1, null.ok = TRUE)

    if (!is.null(model) & is.null(newdata)) {
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


sanitize_vcov <- function(model, vcov) {

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

    # TRUE: try to extract a vcov
    if (isTRUE(vcov)) {
        vcov <- try(get_vcov(model), silent = TRUE)
        if (inherits(vcov, "try-error") && !inherits(model, "brmsfit")) {
            warning(sprintf('Unable to extract a variance-covariance matrix from model of class "%s" using the `stats::vcov` function. The `vcov` argument was switched to `FALSE`. Please supply a named matrix to produce uncertainty estimates.', class(model)[1]))
            return(NULL)
            # dpoMatrix conversion
        }
        vcov <- as.matrix(vcov)
    }

    # align vcov and coefs
    if (is.matrix(vcov)) {
        coefs <- get_coef(model)
        if (anyDuplicated(names(vcov)) == 0) {
            # 1) Check above is needed for `AER::tobit` and others where `vcov`
            # includes Log(scale) but `coef` does not Dangerous for `oridinal::clm`
            # and others where there are important duplicate column names in
            # `vcov`, and selecting with [,] repeats the first instance.

            # 2) Sometimes vcov has more columns than coefs (e.g., betareg)
            if (all(names(coefs) %in% colnames(vcov))) {
                vcov <- vcov[names(coefs), names(coefs), drop = FALSE]
            }
        }
    }

    if (isFALSE(vcov)) {
        vcov <- NULL
    }

    return(vcov)
}


sanity_predict_vector <- function(pred, model, newdata, type) {
    if (!isTRUE(checkmate::check_atomic_vector(pred)) &&
        !isTRUE(checkmate::check_array(pred, d = 1))) {
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
