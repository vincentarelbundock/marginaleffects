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
#' 
#' @template model_specific_arguments
#' @template parallel
#'
#' @examples
#'
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
        checkmate::check_choice(contrast_numeric, choices = c("iqr", "minmax", "sd", "2sd", "dydx")))

    # variables vector
    variables_list <- sanitize_variables(model = model, newdata = newdata, variables = variables)
    variables <- unique(unlist(variables_list, recursive = TRUE))
    # this won't be triggered for multivariate outcomes in `brms`, which
    # produces a list of lists where top level names correspond to names of the
    # outcomes. There should be a more robust way to handle those, but it seems
    # to work for now.
    if ("conditional" %in% names(variables_list)) {
        variables <- intersect(variables, variables_list[["conditional"]])
    }

    # modelbased::visualisation_matrix attaches useful info for plotting
    attributes_newdata <- attributes(newdata)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_newdata) %in% idx
    attributes_newdata <- attributes_newdata[idx]

    # rowid_counterfactual is returned by datagrid(grid.type = "counterfactual")
    # rowid: row in `newdata`
    # rowid_counterfactual: row in the original data counterfactualized by data.grid
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- seq_len(nrow(newdata))
    }

    # compute contrasts and standard errors
    # do.call and dots to avoid unused argument error in future_lapply
    dots <- list(...)
    cache <- get_contrast_data(
        model = model,
        newdata = newdata,
        variables = variables,
        contrast_factor = contrast_factor,
        contrast_numeric = contrast_numeric,
        ...)
    args <- list(model,
                 newdata = newdata,
                 variables = variables,
                 type = type,
                 contrast_factor = contrast_factor,
                 contrast_numeric = contrast_numeric,
                 cache = cache)
    args <- c(args, dots)
    mfx <- do.call("get_contrasts", args)

    # bayesian draws
    if (!is.null(attr(mfx, "posterior_draws"))) {
        draws <- attr(mfx, "posterior_draws")
        J <- NULL

    # standard errors via delta method
    } else if (!is.null(vcov)) {
        idx <- intersect(colnames(mfx), c("type", "group", "term", "contrast"))
        idx <- mfx[, (idx), drop = FALSE]
        args <- list(model,
                     vcov = vcov,
                     type = type,
                     FUN = standard_errors_delta_contrasts,
                     newdata = newdata,
                     index = idx,
                     variables = variables,
                     cache = cache,
                     contrast_factor = contrast_factor,
                     contrast_numeric = contrast_numeric)
        args <- c(args, dots)
        se <- do.call("standard_errors_delta", args)
        mfx$std.error <- as.numeric(se)
        J <- attr(se, "J")
        draws <- NULL

    # no standard error
    } else {
        J <- draws <- NULL
    }

    # meta info
    mfx[["type"]] <- type

    # bayesian posterior draws
    if (!is.null(draws)) {
        if (!"conf.low" %in% colnames(mfx)) {
            tmp <- apply(draws, 1, get_eti)
            mfx[["std.error"]] <- NULL
            mfx[["comparison"]] <- apply(draws, 1, stats::median)
            mfx[["conf.low"]] <- tmp[1, ]
            mfx[["conf.high"]] <- tmp[2, ]
        }
    }

    # DO NOT sort rows because we want draws to match
    row.names(mfx) <- NULL


    # group id: useful for merging, only if it's an internal call and not user-initiated
    if (isTRUE(list(...)$internal_call) && !"group" %in% colnames(mfx)) {
         mfx$group <- "main_marginaleffect"
    }

    # clean columns
    stubcols <- c("rowid", "rowid_counterfactual", "type", "group", "term", "contrast", "comparison", "std.error",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(mfx))
    cols <- unique(c(cols, colnames(mfx)))
    mfx <- mfx[, ..cols, drop = FALSE]

    # merge newdata if requested and restore attributes
    # secret argument
    if (!isTRUE(list(...)[["internal_call"]])) {
        return_data <- sanitize_return_data()
        if (isTRUE(return_data)) {
            mfx <- merge(mfx, newdata, by = "rowid")
        }

        if ("group" %in% colnames(mfx) && all(mfx$group == "main_marginaleffect")) {
            mfx$group <- NULL
        }
    }

    out <- mfx

    if (!isTRUE(list(...)[["return_class"]] == "data.table")) {
        setDF(out)
    }

    class(out) <- c("comparisons", class(out))
    attr(out, "posterior_draws") <- draws
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables
    attr(out, "J") <- J
    attr(out, "vcov") <- vcov

    # modelbased::visualisation_matrix attaches useful info for plotting
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }

    return(out)
}
