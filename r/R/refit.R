#' Refit a marginaleffects object with new data
#'
#' @param object A marginaleffects object (predictions, comparisons, or slopes)
#' @param ... Additional arguments passed to methods
#' @return A marginaleffects object
#' @export
refit <- function(object, ...) {
    UseMethod("refit")
}

#' @rdname refit
#' @param data Optional data frame to refit the underlying model
#' @param newdata Optional data frame to re-evaluate the marginaleffects call
#' @param vcov Optional logical or variance-covariance matrix specification to pass to the marginaleffects call
#' @details
#' If `data` is supplied, the underlying model is refitted using that data.
#' If `newdata` is supplied, the marginaleffects call is re-evaluated with the new data.
#' Both can be supplied together to refit the model and make predictions on new data.
#' @export
refit.marginaleffects <- function(object, data = NULL, newdata = NULL, vcov = NULL, ...) {
    # If both are NULL, return unchanged
    if (is.null(data) && is.null(newdata)) {
        return(object)
    }

    mfx <- attr(object, "marginaleffects")
    if (is.null(mfx)) {
        stop("Object does not have marginaleffects attributes")
    }

    model <- mfx@model

    fit_again <- function(model, data) {
        # Try stats::update first
        model <- tryCatch(
            stats::update(model, data = data),
            error = function(e) NULL
        )
        # Fallback: modify call and re-evaluate
        if (is.null(model)) {
            if (is.call(mfx@call_model) && "data" %in% names(mfx@call_model)) {
                call_new <- mfx@call_model
                call_new$data <- data
                model <- try(eval(call_new), silent = TRUE)
                if (inherits(model, "try-error")) {
                    stop("Failed to refit the model.", call. = FALSE)
                }
            } else {
                stop("Failed to refit model: no update method available", call. = FALSE)
            }
        }
        return(model)
    }

    # Step 1: Refit model if data is supplied
    if (!is.null(data)) {
        # For workflows, tidymodels provides its own fit.workflow method
        if (inherits(model, "workflow")) {
            model <- generics::fit(model, data = data)
        } else if (inherits(model, "model_fit")) {
            model <- fit_again(model[["fit"]], data = data)
        } else {
            model <- fit_again(model, data = data)
        }
    }

    # Step 2: Re-evaluate marginaleffects call
    call_new <- mfx@call
    call_new[["model"]] <- model

    if (!is.null(newdata)) {
        call_new[["newdata"]] <- newdata
    }

    if (!is.null(vcov)) {
        call_new[["vcov"]] <- vcov
    }

    result <- eval(call_new)
    return(result)
}

#' @rdname refit
#' @export
refit.predictions <- refit.marginaleffects

#' @rdname refit
#' @export
refit.comparisons <- refit.marginaleffects

#' @rdname refit
#' @export
refit.slopes <- refit.marginaleffects

#' @rdname refit
#' @export
refit.hypotheses <- refit.marginaleffects
