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
#' @param contrast_factor string
#' * "reference": Each factor level is compared to the factor reference (base) level
#' * "sequential": Each factor level is compared to the previous factor level
#' * "pairwise": Each factor level is compared to all other levels
#' @param contrast_numeric string or numeric
#' * Numeric of length 1: Contrast between the observed value and the observed value plus `contrast_numeric`
#' * Numeric vector of length 2: Contrast between the 2nd element and the 1st element of the `contrast_numeric` vector.
#' * "iqr": Contrast across the interquartile range of the regressor.
#' * "sd": Contrast across one standard deviation around the regressor mean.
#' * "2sd": Contrast across two standard deviations around the regressor mean.
#' * "minmax": Contrast between the maximum and the minimum values of the regressor.
#' @examples

#' library(marginaleffects)
#' library(magrittr)
#'
#' # Linear model
#' tmp <- mtcars
#' tmp$am <- as.logical(tmp$am)
#' mod <- lm(mpg ~ am + factor(cyl), tmp)
#' comparisons(mod, contrast_factor = "reference") %>% tidy()
#' comparisons(mod, contrast_factor = "sequential") %>% tidy()
#' comparisons(mod, contrast_factor = "pairwise") %>% tidy()
#'
#' # GLM with different scale types
#' mod <- glm(am ~ factor(gear), data = mtcars)
#' comparisons(mod) %>% tidy()
#' comparisons(mod, type = "link") %>% tidy()
#'
#' # Numeric contrasts
#' mod <- lm(mpg ~ hp, data = mtcars)
#' comparisons(mod, contrast_numeric = 1) %>% tidy()
#' comparisons(mod, contrast_numeric = 5) %>% tidy()
#' comparisons(mod, contrast_numeric = c(90, 100)) %>% tidy()
#' comparisons(mod, contrast_numeric = "iqr") %>% tidy()
#' comparisons(mod, contrast_numeric = "sd") %>% tidy()
#' comparisons(mod, contrast_numeric = "minmax") %>% tidy()
#'
#' @export
comparisons <- function(model,
                        variables = NULL,
                        newdata = NULL,
                        type = "response",
                        vcov = TRUE,
                        contrast_factor = "reference",
                        contrast_numeric = 1,
                        ...) {

    if (!isTRUE(list(...)[["internal_call"]])) {
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
        vcov <- sanitize_vcov(model, vcov)
    }

    # TODO: don't run sanity checks if this is an internal call. But
    # this can create problems.
    # secret argument
    model <- sanity_model(model = model, ...)
    sanity_type(model = model, type = type)
    checkmate::assert_choice(contrast_factor, choices = c("reference", "sequential", "pairwise"))
    checkmate::assert(
        checkmate::check_numeric(contrast_numeric, min.len = 1, max.len = 2),
        checkmate::check_choice(contrast_numeric, choices = c("iqr", "minmax", "sd", "2sd")))

    # variables vector
    variables_list <- sanity_variables(model = model, newdata = newdata, variables = variables)
    variables <- unique(unlist(variables_list))
    # this won't be triggered for multivariate outcomes in `brms`, which
    # produces a list of lists where top level names correspond to names of the
    # outcomes. There should be a more robust way to handle those, but it seems
    # to work for now.
    if ("conditional" %in% names(variables)) {
        variables <- intersect(variables, variables[["conditional"]])
    }

    # modelbased::visualisation_matrix attaches useful info for plotting
    attributes_newdata <- attributes(newdata)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_newdata) %in% idx
    attributes_newdata <- attributes_newdata[idx]

    # Create counterfactual datasets with different factor values and compare the predictions
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    mfx_list <- se_mean_list <- draws_list <- J_list <- J_mean_list <- list()

    for (variable in variables) {
        for (predt in type) {
            mfx <- get_contrasts(model,
                                 newdata = newdata,
                                 variable = variable,
                                 type = predt,
                                 contrast_factor = contrast_factor,
                                 contrast_numeric = contrast_numeric,
                                 ...)
            # bayesian draws
            if (!is.null(attr(mfx, "posterior_draws"))) {
                draws_list <- c(draws_list, list(attr(mfx, "posterior_draws")))
                J <- J_mean <- NULL

            # standard errors via delta method
            } else if (!is.null(vcov)) {
                idx <- intersect(colnames(mfx), c("type", "group", "term", "contrast"))
                idx <- mfx[, idx, drop = FALSE]
                se <- standard_errors_delta(model,
                                            vcov = vcov,
                                            type = predt,
                                            FUN = standard_errors_delta_contrasts,
                                            newdata = newdata,
                                            index = idx,
                                            variable = variable,
                                            contrast_factor = contrast_factor,
                                            contrast_numeric = contrast_numeric,
                                            ...)
                mfx$std.error <- as.numeric(se)
                J <- attr(se, "J")
                J_mean <- attr(se, "J_mean")

            # no standard error
            } else {
                J <- J_mean <- NULL
            }

            mfx_list <- c(mfx_list, list(mfx))
            J_list <- c(J_list, list(J))
            J_mean_list <- c(J_mean_list, list(J_mean))
        }
    }


    out <- bind_rows(mfx_list)

    # duplicate colnames can occur for grouped outcome models, so we can't just
    # use `poorman::bind_rows()`. Instead, ugly hack to make colnames unique
    # with a weird string.
    for (i in seq_along(J_mean_list)) {
        if (inherits(J_mean_list[[i]], "data.frame")) {
            newnames <- make.unique(names(J_mean_list[[i]]), sep = "______")
            J_mean_list[[i]] <- stats::setNames(J_mean_list[[i]], newnames)
        }
    }
    J_mean <- bind_rows(J_mean_list) # bind_rows need because some have contrast col
    if (inherits(J_mean, "data.frame")) {
        J_mean <- stats::setNames(J_mean, gsub("______.*$", "", colnames(J_mean)))
    }


    # empty contrasts equal "". important for merging in `tidy()`
    if ("contrast" %in% colnames(J_mean)) {
        J_mean$contrast <- ifelse(is.na(J_mean$contrast), "", J_mean$contrast)
    }

    # standard error at mean gradient (this is what Stata and R's `margins` compute)
    # J_mean is NULL in bayesian models and where the delta method breaks
    if (!is.null(J_mean) && !is.null(vcov)) {
        idx <- !colnames(J_mean) %in% c("type", "group", "term", "contrast")
        tmp <- J_mean[, !idx, drop = FALSE]
        J_mean_mat <- as.matrix(J_mean[, idx, drop = FALSE])
        # converting to data.frame can sometimes break colnames
        colnames(J_mean_mat) <- colnames(J)
        # aggressive check. probably needs to be relaxed.
        if (any(colnames(J_mean_mat) != colnames(vcov))) {
            tmp <- NULL
            warning("The variance covariance matrix and the Jacobian do not match. `marginaleffects` is unable to compute standard errors using the delta method.")
        } else {
            V <- colSums(t(J_mean_mat %*% vcov) * t(J_mean_mat))
            tmp$std.error <- sqrt(V)
        }
        se_at_mean_gradient <- tmp
    } else {
        se_at_mean_gradient <- NULL
    }

    # bayesian posterior draws
    draws <- do.call("rbind", draws_list)
    if (!is.null(draws)) {
        if (!"conf.low" %in% colnames(out)) {
            tmp <- apply(draws, 1, get_hdi)
            out[["std.error"]] <- NULL
            out[["comparison"]] <- apply(draws, 1, stats::median)
            out[["conf.low"]] <- tmp[1, ]
            out[["conf.high"]] <- tmp[2, ]
        }
    }

    # # merge newdata if requested and restore attributes
    # return_data <- sanitize_return_data()
    # if (isTRUE(return_data)) {
    #     out <- left_join(out, newdata, by = "rowid")
    # }

    # DO NOT sort rows because we want draws to match
    row.names(out) <- NULL

    # group id: useful for merging, only if it's an internal call and not user-initiated
    if (isTRUE(list(...)$internal_call) && !"group" %in% colnames(out)) {
         out$group <- "main_marginaleffect"
    }

    # clean columns
    stubcols <- c("rowid", "type", "group", "term", "contrast", "comparison", "std.error",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols]

    # we want consistent output, regardless of whether `data.table` is installed/used or not
    out <- as.data.frame(out)

    # merge newdata if requested and restore attributes
    # secret argument
    if (!isTRUE(list(...)[["internal_call"]])) {
        return_data <- sanitize_return_data()
        if (isTRUE(return_data)) {
            out <- left_join(out, newdata, by = "rowid")
        }

        if ("group" %in% colnames(out) && all(out$group == "main_marginaleffect")) {
            out$group <- NULL
        }
    }

    class(out) <- c("comparisons", class(out))
    attr(out, "posterior_draws") <- draws
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables
    attr(out, "J") <- J
    attr(out, "J_mean") <- J_mean
    attr(out, "se_at_mean_gradient") <- se_at_mean_gradient

    # modelbased::visualisation_matrix attaches useful info for plotting
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }

    return(out)
}
