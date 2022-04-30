#' check if dependency is installed
#'
#' @noRd
check_dependency <- function(library_name) {
  flag <- requireNamespace(library_name, quietly = TRUE)
  if (!isTRUE(flag)) {
      msg <- sprintf("Please install the `%s` package.", library_name)
      return(msg)
  } else {
      return(TRUE)
  }
}
assert_dependency <- checkmate::makeAssertionFunction(check_dependency)



sanity_newdata <- function(model, newdata) {
    checkmate::check_data_frame(newdata,
                                null.ok = TRUE,
                                any.missing = FALSE)


    if (is.null(newdata)) {
        newdata <- insight::get_data(model)
    }

    # required for the type of column indexing to follow
    data.table::setDF(newdata)

    # rbindlist breaks on matrix columns
    idx <- sapply(newdata, function(x) class(x)[1] != "matrix")
    newdata <- newdata[, idx, drop = FALSE]

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

    # we will need this to merge the original data back in, and it is better to
    # do it in a centralized upfront way.
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    return(newdata)
}

sanitize_variables <- function(model, newdata, variables) {
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

    # mhurdle names the variables weirdly
    if (inherits(model, "mhurdle")) {
        variables_list <- list("conditional" = insight::find_predictors(model, flatten = TRUE))
    }

    # weights
    # HACK: find_weights sometimes mysteriously fails on brms models
    if (!is.null(model) && !inherits(model, "brmsfit") && !inherits(model, "stanreg")) {
        w <- tryCatch(insight::find_weights(model), error = function(e) NULL)
        w <- intersect(w, colnames(newdata))
        variables <- unique(c(variables, w))
        variables_list[["weights"]] <- w
    }

    # check missing character levels
    # Character variables are treated as factors by model-fitting functions,
    # but unlike factors, they do not not keep a record of all factor levels.
    # This poses problem when feeding `newdata` to `predict`, which often
    # breaks (via `model.matrix`) when the data does not include all possible
    # factor levels.
    levels_character <- list()
    for (v in variables) {
        if (v %in% colnames(origindata) && is.character(origindata[[v]])) {
            levels_character[[v]] <- unique(origindata[[v]])
        }
    }
    attr(variables_list, "levels_character") <- levels_character

    # check missing variables
    miss <- setdiff(variables, colnames(newdata))
    if (length(miss) > 0) {
        stop(sprintf("Variables missing from `newdata` and/or the data extracted from the model objects: %s",
                     paste(miss, collapse = ", ")),
             call. = FALSE)
    }

    return(variables_list)
}


sanity_predict_vector <- function(pred, model, newdata, type) {
    if (!isTRUE(checkmate::check_atomic_vector(pred)) &&
        !isTRUE(checkmate::check_array(pred, d = 1))) {
        msg <- sprintf(
'`predict(model, type = "%s")` was called on a model of class `%s`, but this command did not produce the expected outcome: A numeric vector of length %s. This can sometimes happen when users try compute a marginal effect for some models with grouped or multivariate outcome which are not supported yet by `marginaleffects` package. Please consult your modeling package documentation to learn what alternative `type` arguments are accepted by the `predict` method, or file a feature request on Github:  https://github.com/vincentarelbundock/marginaleffects/issues',
        type, class(model)[1], nrow(newdata))
        stop(msg, call. = FALSE)
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



# OBSOLETE CHECKS KEPT FOR POSTERITY

# sanitize_return_data <- function() {
#     return_data <- getOption("marginaleffects_return_data", default = TRUE)
#     checkmate::assert_flag(return_data)
#     return(return_data)
# }


# sanitize_numDeriv_method <- function() {
#     numDeriv_method <- getOption("marginaleffects_numDeriv_method", default = "simple")
#     checkmate::assert_choice(numDeriv_method, choices = c("simple", "complex", "Richardson"))
#     return(numDeriv_method)
# }
