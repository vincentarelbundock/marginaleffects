#' Experimental function to compute contrasts between adjusted predictions
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
#' @param contrast_factor "reference" or "sequential"
#' * "reference": Each factor level is compared to the factor reference (base) level
#' * "sequential": Each factor level is compared to the previous factor level
#' * "revsequential": Same as "sequential" but in the reverse order
#' * "pairwise": Each factor level is compared to all other levels
#' * "revpairwise": Same as pairwise, but with the reverse order of subtraction
#' @param contrast_numeric string or numeric
#' * Numeric of length 1: Contrast between the observed value and the observed value plus `contrast_numeric`
#' * Numeric vector of length 2: Contrast between the 2nd element and the 1st element of the `contrast_numeric` vector.
#' * "iqr": Contrast across the interquartile range of the regressor.
#' * "sd": Contrast across one standard deviation around the regressor mean.
#' * "2sd": Contrast across two standard deviations around the regressor mean.
#' * "minmax": Contrast between the maximum and the minimum values of the regressor.
#' @export
comparisons <- function(model,
                        variables = NULL,
                        newdata = insight::get_data(model),
                        type = "response",
                        vcov = TRUE,
                        contrast_factor = "reference",
                        contrast_numeric = 1,
                        ...) {


    sanity_newdata(model = model, newdata = newdata)
    sanity_type(model = model, type = type)
    variables <- unlist(sanity_variables(model = model, newdata = newdata, variables = variables))
    checkmate::assert_choice(contrast_factor, choices = c("reference", "sequential", "revsequential", "pairwise", "revpairwise"))
    checkmate::assert(
        checkmate::check_numeric(contrast_numeric, min.len = 1, max.len = 2),
        checkmate::check_choice(contrast_numeric, choices = c("iqr", "minmax", "sd", "2sd")))

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

    out_list <- list()

    for (predt in type) {
        for (variable in variables) {
            # logical and character before factor, because they get picked up by find_categorical
            if (is.logical(newdata[[variable]])) {
                out_list[[variable]] <- get_contrasts_logical(
                    model = model,
                    newdata = newdata,
                    variable = variable,
                    type = predt,
                    ...)

            } else if (is.character(newdata[[variable]])) {
                out_list[[variable]] <- get_contrasts_character(
                    model = model,
                    newdata = newdata,
                    variable = variable,
                    type = predt,
                    ...)

            } else if (is.factor(newdata[[variable]]) ||
                       variable %in% find_categorical(newdata = newdata, model = model) ||
                       isTRUE(attr(newdata[[variable]], "factor"))) {
               out_list[[variable]] <- get_contrasts_factor(
                    model = model,
                    newdata = newdata,
                    type = predt,
                    variable = variable,
                    contrast_factor = contrast_factor,
                    ...)

            } else if (is.numeric(newdata[[variable]])) {
                out_list[[variable]] <- get_contrasts_numeric(
                    model = model,
                    newdata = newdata,
                    variable = variable,
                    type = predt,
                    contrast_numeric = contrast_numeric,
                    ...)

            } else {
                stop(sprintf("Cannot compute contrasts for variable %s of class %s",
                             variable,
                             class(newdata[[variable]])))
            }
        }
    }

    out <- bind_rows(out_list)

    # required for merging in models with multiple response levels
    if (!"group" %in% colnames(out)) {
        out$group <- "main_marginaleffect"
    }

    class(out) <- c("comparisons", class(out))

    return(out)
}
