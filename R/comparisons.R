#' Contrasts between adjusted predictions
#'
#' This function calculates contrasts (or comparisons) between adjusted
#' predictions for each row of the dataset. The resulting object can processed
#' by the `tidy()` or `summary()` functions, which compute Average Contrasts.
#' The `datagrid()` function and the `newdata` argument can be used to
#' calculate contrasts Contrasts at the Mean or Contrasts at User-Specified
#' values (aka Contrasts at Representative values). Additional information can
#' be found in the Details and Examples sections below, and in the vignette on
#' the `marginaleffects` website.
#'
#' A "contrast" is the difference between two adjusted predictions, calculated
#' for meaningfully different regressor values (e.g., College graduates vs.
#' Others). Uncertainty estimates are computed using the delta method.
#'
#' Detailed vignettes on contrasts, marginal effects, predictions, and marginal
#' means, as well as a list of supported models can be found on the package
#' website:
#'
#' https://vincentarelbundock.github.io/marginaleffects/
#'
#' @inheritParams marginaleffects
#' @export
comparisons <- function(model,
                        variable,
                        newdata = insight::get_data(model),
                        type = "response",
                        contrast_factor = "reference",
                        contrast_numeric = 1,
                        contrast_numeric_slope = FALSE,
                        ...) {

    # if `newdata` is a call to `datagrid`, `typical`, or `counterfactual`, insert `model`
    scall <- substitute(newdata)
    if (is.call(scall) && as.character(scall)[1] %in% c("datagrid", "typical", "counterfactual")) {
        lcall <- as.list(scall)
        if (!any(c("model", "data") %in% names(lcall))) {
            lcall <- c(lcall, list("model" = model))
            newdata <- eval.parent(as.call(lcall))
        }
    }
    newdata <- sanity_newdata(model, newdata)

    # Create counterfactual datasets with different factor values and compare the predictions
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    # logical and character before factor, because they get picked up by find_categorical
    if (is.logical(newdata[[variable]])) {
        out <- get_contrasts_logical(
            model = model,
            newdata = newdata,
            variable = variable,
            type = type,
            ...)

    } else if (is.character(newdata[[variable]])) {
        out <- get_contrasts_character(
            model = model,
            newdata = newdata,
            variable = variable,
            type = type,
            ...)

    } else if (is.factor(newdata[[variable]]) || variable %in% find_categorical(newdata = newdata, model = model) || isTRUE(attr(newdata[[variable]], "factor"))) {
       out <- get_contrasts_factor(
            model = model,
            newdata = newdata,
            type = type,
            variable = variable,
            contrast_factor = contrast_factor,
            ...)

    } else if (is.numeric(newdata[[variable]])) {
        out <- get_contrasts_numeric(
            model = model,
            newdata = newdata,
            variable = variable,
            type = type,
            contrast_numeric = contrast_numeric,
            contrast_numeric_slope = contrast_numeric_slope,
            ...)

    } else {
        stop(sprintf("Cannot compute contrasts for variable %s of class %s",
                     variable,
                     class(newdata[[variable]])))
    }

    # required for merging in models with multiple response levels
    if (!"group" %in% colnames(out)) {
        out$group <- "main_marginaleffect"
    }

    return(out)
}
