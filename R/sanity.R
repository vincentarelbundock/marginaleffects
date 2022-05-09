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


    # if (inherits(model, "mlogit") && !is.null(newdata)) {
    #     stop("The `newdata` argument is not supported for models of class `mlogit`.", call. = FALSE)
    # }

    if (is.null(newdata)) {
        newdata <- suppressWarnings(insight::get_data(model))
    }

    if (!inherits(newdata, "data.frame")) {
        msg <- sprintf("Unable to extract the data from model of class `%s`.", class(model)[1])
        stop(msg, call. = FALSE)
    }

    # required for the type of column indexing to follow
    data.table::setDF(newdata)

    # mlogit: each row is an individual-choice, but the index is not easily
    # trackable, so we pre-sort it here, and the sort in `get_predict()`. We
    # need to cross our fingers, but this probably works.
    if (inherits(model, "mlogit") && isTRUE(inherits(newdata[["idx"]], "idx"))) {
        idx <- list(newdata[["idx"]][, 1], newdata[["idx"]][, 2])
        newdata <- newdata[order(newdata[["idx"]][, 1], newdata[["idx"]][, 2]),]
    }

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


sanity_contrast_numeric <- function(contrast_numeric, assertion = TRUE) {
    flag <- isTRUE(checkmate::check_numeric(contrast_numeric, min.len = 1, max.len = 2)) ||
            isTRUE(checkmate::check_choice(contrast_numeric, choices = c("iqr", "minmax", "sd", "2sd", "dydx")))
    if (isTRUE(assertion) && !isTRUE(flag)) {
        stop('Contrasts for numeric variables can be a single numeric value, a numeric vector of length 2, or one of the following strings: "iqr", "minmax", "sd", "2sd", "dydx"', call. = FALSE)
    } else {
        return(flag)
    }
}


sanity_contrast_factor <- function(contrast_factor, assertion = TRUE) {
    flag <- checkmate::check_choice(
        contrast_factor,
        choices = c("reference", "sequential", "pairwise", "all"))
    if (isTRUE(assertion) && !isTRUE(flag)) {
        stop('Contrasts for factor or character variables can be: "reference", "sequential", "pairwise", or "all".', call. = FALSE)
    } else {
        return(flag)
    }
}


sanitize_interaction <- function(interaction, variables, model) {

    checkmate::assert_flag(interaction, null.ok = TRUE)

    if (isTRUE(interaction) && is.null(variables)) {
        msg <- "When `interaction=TRUE` you must use the `variables` argument to specify which variables should be interacted."
        stop(msg, call. = TRUE)
    }

    if (isTRUE(checkmate::check_flag(interaction))) {
        return(interaction)
    }

    inter <- try(insight::find_interactions(model, flatten = TRUE), silent = TRUE)
    if (!is.null(variables) && isTRUE(length(inter) > 0)) {
        return(TRUE)
    } else {
        return(FALSE)
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


