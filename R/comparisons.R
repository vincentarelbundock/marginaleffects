#' Contrasts Between Adjusted Predictions
#'
#' Difference, ratio, or function of adjusted predictions, calculated for
#' meaningfully different predictor values. The `tidy()` and `summary()`
#' functions can be used to aggregate and summarize the output of
#' `comparisons()`. To learn more, read the contrasts vignette, visit the
#' package website, or scroll down this page for a full list of vignettes:
#' * <https://vincentarelbundock.github.io/marginaleffects/articles/contrasts.html>
#' * <https://vincentarelbundock.github.io/marginaleffects/>
#'
#' @section Vignettes and documentation:
#'
#' ```{r child = "vignettes/toc.Rmd"}
#' ```
#'
#' @details
#' A "contrast" is a difference, ratio of function of adjusted predictions,
#' calculated for meaningfully different predictor values (e.g., College
#' graduates vs. Others). Uncertainty estimates are computed using the delta
#' method.
#'
#' The `newdata` argument can be used to control the kind of contrasts to report:
#' 
#' * Average Contrasts
#' * Adjusted Risk Ratios
#' * Adjusted Risk Differences
#' * Group-Average Contrasts
#' * Contrasts at the Mean
#' * Contrasts at User-Specified values (aka Contrasts at Representative values, MER).
#' * Custom contrasts using arbitrary functions
#'
#' @inheritParams marginaleffects
#' @inheritParams predictions
#' @param variables `NULL`, character vector, or named list. The subset of variables for which to compute contrasts.
#' * `NULL`: compute contrasts for all the variables in the model object (can be slow).
#' * Character vector: subset of variables (usually faster).
#' * Named list: names identify the subset of variables of interest, and values define the type of contrast to compute. Acceptable values depend on the variable type:
#'   - Factor or character variables:
#'     * "reference": Each factor level is compared to the factor reference (base) level
#'     * "all": All combinations of observed levels
#'     * "sequential": Each factor level is compared to the previous factor level
#'     * "pairwise": Each factor level is compared to all other levels
#'   - Logical variables:
#'     * NULL: contrast between TRUE and FALSE
#'   - Numeric variables:
#'     * Numeric of length 1: Contrast for a gap of `x`, computed at the observed value plus and minus `x / 2`
#'     * Numeric vector of length 2: Contrast between the 2nd element and the 1st element of the `x` vector.
#'     * "iqr": Contrast across the interquartile range of the regressor.
#'     * "sd": Contrast across one standard deviation around the regressor mean.
#'     * "2sd": Contrast across two standard deviations around the regressor mean.
#'     * "minmax": Contrast between the maximum and the minimum values of the regressor.
#'   - Examples:
#'     + `variables = list(gear = "pairwise", hp = 10)`
#'     + `variables = list(gear = "sequential", hp = c(100, 120))`
#'     + See the Examples section below for more.
#' @param newdata `NULL`, data frame, string, or `datagrid()` call. Determines the predictor values for which to compute contrasts.
#' + `NULL` (default): Unit-level contrasts for each observed value in the original dataset.
#' + data frame: Unit-level contrasts for each row of the `newdata` data frame.
#' + string:
#'   - "mean": Contrasts at the Mean. Contrasts when each predictor is held at its mean or mode.
#'   - "median": Contrasts at the Median. Contrasts when each predictor is held at its median or mode.
#'   - "marginalmeans": Contrasts at Marginal Means.
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid] documentation.
#' @param transform_pre (experimental) string or function. How should pairs of adjusted predictions be contrasted?
#' * string: shortcuts to common contrast functions.
#'   - "difference" (default): `function(hi, lo) hi - lo`
#'   - "differenceavg": `function(hi, lo) mean(hi) - mean(lo)`
#'   - "ratio": `function(hi, lo) hi / lo`
#'   - "lnratio": `function(hi, lo) log(hi / lo)`
#'   - "ratioavg": `function(hi, lo) mean(hi) / mean(lo)`
#'   - "lnratioavg": `function(hi, lo) log(mean(hi) / mean(lo))`
#'   - "lnoravg": `function(hi, lo) log((mean(hi)/(1 - mean(hi))) / (mean(lo)/(1 - mean(lo))))`
#'   - "dydx": `function(hi, lo) (hi - lo) / eps`
#'   - "dydx": `function(hi, lo) mean((hi - lo) / eps)'
#'   - "expdydx": `function(hi, lo) ((exp(hi) - exp(lo)) / exp(e)) / e`
#' * function: accept two equal-length numeric vectors of adjusted predictions (`hi` and `lo`) and returns a vector of contrasts of the same length, or a unique numeric value.
#' @param transform_post (experimental) A function applied to unit-level estimates and confidence intervals just before the function returns results.
#' @param interaction TRUE, FALSE, or NULL
#' * `FALSE`: Contrasts represent the change in adjusted predictions when one predictor changes and all other variables are held constant.
#' * `TRUE`: Contrasts represent the changes in adjusted predictions when the predictors specified in the `variables` argument are manipulated simultaneously.
#' * `NULL` (default): Behaves like `TRUE` when the `variables` argument is specified and the model formula includes interactions. Behaves like `FALSE` otherwise.
#' @template model_specific_arguments
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
#' comparisons(mod, variables = list(cyl = "reference")) %>% tidy()
#' comparisons(mod, variables = list(cyl = "sequential")) %>% tidy()
#' comparisons(mod, variables = list(cyl = "pairwise")) %>% tidy()
#'
#' # GLM with different scale types
#' mod <- glm(am ~ factor(gear), data = mtcars)
#' comparisons(mod, type = "response") %>% tidy()
#' comparisons(mod, type = "link") %>% tidy()
#'
#' # Contrasts at the mean
#' comparisons(mod, newdata = "mean")
#'
#' # Contrasts between marginal means
#' comparisons(mod, newdata = "marginalmeans")
#'
#' # Contrasts at user-specified values
#' comparisons(mod, newdata = datagrid(am = 0, cyl = tmp$cyl))
#'
#' # Numeric contrasts
#' mod <- lm(mpg ~ hp, data = mtcars)
#' comparisons(mod, variables = list(hp = 1)) %>% tidy()
#' comparisons(mod, variables = list(hp = 5)) %>% tidy()
#' comparisons(mod, variables = list(hp = c(90, 100))) %>% tidy()
#' comparisons(mod, variables = list(hp = "iqr")) %>% tidy()
#' comparisons(mod, variables = list(hp = "sd")) %>% tidy()
#' comparisons(mod, variables = list(hp = "minmax")) %>% tidy()
#'
#' # Adjusted Risk Ratio: see the contrasts vignette
#' mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
#' cmp <- comparisons(mod, transform_pre = "lnratioavg")
#' summary(cmp, transform_post = exp)
#'
#' # Adjusted Risk Ratio: Manual specification of the `transform_pre`
#' cmp <- comparisons(mod, transform_pre = function(hi, lo) log(mean(hi) / mean(lo)))
#' summary(cmp, transform_post = exp)
#
#' # Interactions between contrasts
#' mod <- lm(mpg ~ factor(cyl) * factor(gear) + hp, data = mtcars)
#' cmp <- comparisons(mod, variables = c("cyl", "gear"))
#' summary(cmp)
#'
#' # variable-specific contrasts
#' cmp <- comparisons(mod, variables = list(gear = "sequential", hp = 10))
#' summary(cmp)
#'
#' # hypothesis test: is the `hp` marginal effect at the mean equal to the `drat` marginal effect
#' mod <- lm(mpg ~ wt + drat, data = mtcars)
#'
#' comparisons(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = "wt = drat")
#' 
#' # same hypothesis test using row indices
#' comparisons(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = "b1 - b2 = 0")
#' 
#' # same hypothesis test using numeric vector of weights
#' comparisons(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = c(1, -1))
#' 
#' # two custom contrasts using a matrix of weights
#' lc <- matrix(c(
#'     1, -1,
#'     2, 3),
#'     ncol = 2)
#' comparisons(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = lc)
#' 
#' @export
comparisons <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        type = "response",
                        vcov = TRUE,
                        conf_level = 0.95,
                        transform_pre = "difference",
                        transform_post = NULL,
                        interaction = NULL,
                        wts = NULL,
                        hypothesis = NULL,
                        ...) {

    dots <- list(...)

    # marginaleffects()` **must** run its own sanity checks and hardcode valid arguments
    internal_call <- dots[["internal_call"]]
    if (!isTRUE(internal_call)) {
        # if `newdata` is a call to `datagrid`, `typical`, or `counterfactual`, insert `model`
        # should probably not be nested too deeply in the call stack since we eval.parent() (not sure about this)
        scall <- substitute(newdata)
        if (is.call(scall) && as.character(scall)[1] %in% c("datagrid", "typical", "counterfactual")) {
            lcall <- as.list(scall)
            if (!any(c("model", "data") %in% names(lcall))) {
                lcall <- c(lcall, list("model" = model))
                newdata <- eval.parent(as.call(lcall))
            }

        } else {
            if (is.null(newdata) && !is.null(hypothesis)) {
                newdata <- "mean"
                msg <- format_msg(
                'The `hypothesis` argument of the `comparisons()` function must be used in
                conjunction with the `newdata` argument. `newdata` was switched from NULL to
                "mean" automatically.')
                warning(msg, call. = FALSE)
            }
        }

        model <- sanitize_model(
            model = model, newdata = newdata, wts = wts,
            calling_function = "comparisons", ...)
        conf_level <- sanitize_conf_level(conf_level, ...)
        interaction <- sanitize_interaction(interaction, variables, model)
        sanitize_type(model = model, type = type)
        checkmate::assert_function(transform_post, null.ok = TRUE)
        if ("eps" %in% names(dots)) {
            stop("The `eps` argument is only supported by the `marginaleffects()` function.", call. = FALSE)
        }
    }

    hypothesis <- sanitize_hypothesis(hypothesis, ...)


    # step size is only used and sanitized by `marginaleffects()`
    if ("eps" %in% names(dots)) {
        eps <- dots[["eps"]]
        dots[["eps"]] <- NULL
    } else {
        eps <- list(default_eps = 1e-4)
    }

    marginalmeans <- isTRUE(checkmate::check_choice(newdata, choices = "marginalmeans")) # before sanitize_newdata
    newdata <- sanity_newdata(model = model, newdata = newdata)

    # matrix columns not supported
    matrix_columns <- attr(newdata, "matrix_columns")
    if (any(matrix_columns %in% c(names(variables), variables))) {
        msg <- "Matrix columns are not supported by the `variables` argument."
        stop(msg, call. = FALSE)
    }

    # transformation labels (before sanitation)
    transform_pre_label <- transform_post_label <- NULL
    if (is.function(transform_pre)) {
        transform_pre_label <- deparse(substitute(transform_pre))
    }
    if (!is.null(transform_post)) {
        transform_post_label <- deparse(substitute(transform_post))
    }
    transform_pre <- sanitize_transform_pre(transform_pre)


    # deprecated arguments still used internally and should be kept for backward compatibility
    if ("contrast_factor" %in% names(dots)) {
        contrast_factor <- dots[["contrast_factor"]]
        dots[["contrast_factor"]] <- NULL
    } else {
        contrast_factor <- "reference"
    }
    if ("contrast_numeric" %in% names(dots)) {
        contrast_numeric <- dots[["contrast_numeric"]]
        dots[["contrast_numeric"]] <- NULL
    } else {
        contrast_numeric <- 1
    }
    sanity_contrast_factor(contrast_factor) # hardcoded in marginaleffects()
    sanity_contrast_numeric(contrast_numeric) # hardcoded in marginaleffects()



    # weights
    sanity_wts(wts, newdata) # after sanity_newdata
    if (!is.null(wts) && isTRUE(checkmate::check_string(wts))) {
        newdata[["marginaleffects_wts_internal"]] <- newdata[[wts]]
    } else {
        newdata[["marginaleffects_wts_internal"]] <- wts
    }

    # get dof before transforming the vcov arg
    if (is.character(vcov) && (isTRUE(vcov == "satterthwaite") || isTRUE(vcov == "kenward-roger"))) {
        mi <- insight::model_info(model)
        V <- get_vcov(model, vcov = vcov)
        df <- insight::find_response(model)
        # predict.lmerTest requires the DV
        if (!df %in% colnames(newdata)) {
            newdata[[df]] <- mean(insight::get_response(model))
        }
        dof <- insight::get_df(model, type = vcov, data = newdata)
    } else {
        dof <- NULL
    }

    vcov.type <- get_vcov_label(vcov)
    vcov <- get_vcov(model, vcov = vcov)

    # variables vector
    variables_list <- sanitize_variables(model = model, newdata = newdata, variables = variables)
    contrast_types <- attr(variables_list, "contrast_types")
    variables_vec <- unique(unlist(variables_list, recursive = TRUE))
    # this won't be triggered for multivariate outcomes in `brms`, which
    # produces a list of lists where top level names correspond to names of the
    # outcomes. There should be a more robust way to handle those, but it seems
    # to work for now.
    if ("conditional" %in% names(variables_list)) {
        variables_vec <- intersect(variables_vec, variables_list[["conditional"]])
    }

    # matrix columns are not supported
    variables_vec <- setdiff(variables_vec, matrix_columns)

    # modelbased::visualisation_matrix attaches useful info for plotting
    attributes_newdata <- attributes(newdata)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_newdata) %in% idx
    attributes_newdata <- attributes_newdata[idx]

    # compute contrasts and standard errors
    args <- list(model = model,
                 newdata = newdata,
                 variables = variables_vec,
                 contrast_factor = contrast_factor,
                 contrast_numeric = contrast_numeric,
                 interaction = interaction,
                 contrast_types = contrast_types,
                 marginalmeans = marginalmeans,
                 contrast_label = transform_pre[["label"]],
                 eps = eps)
    args <- c(args, dots)
    cache <- do.call("get_contrast_data", args)

    args <- list(model,
                 newdata = newdata,
                 variables = variables_vec,
                 type = type,
                 transform_pre = transform_pre[["function"]],
                 contrast_factor = contrast_factor,
                 contrast_numeric = contrast_numeric,
                 eps = eps,
                 cache = cache,
                 marginalmeans = marginalmeans,
                 hypothesis = hypothesis)
    args <- c(args, dots)
    mfx <- do.call("get_contrasts", args)

    # bayesian posterior
    if (!is.null(attr(mfx, "posterior_draws"))) {
        draws <- attr(mfx, "posterior_draws")
        J <- NULL

    # standard errors via delta method
    } else if (isTRUE(checkmate::check_matrix(vcov))) {
        idx <- intersect(colnames(mfx), c("type", "group", "term", "contrast"))
        idx <- mfx[, (idx), drop = FALSE]
        args <- list(model,
                     vcov = vcov,
                     type = type,
                     FUN = get_se_delta_contrasts,
                     newdata = newdata,
                     index = idx,
                     variables = variables_vec,
                     cache = cache,
                     transform_pre = transform_pre[["function"]],
                     contrast_factor = contrast_factor,
                     contrast_numeric = contrast_numeric,
                     marginalmeans = marginalmeans,
                     hypothesis = hypothesis,
                     eps = 1e-4)
        args <- c(args, dots)
        se <- do.call("get_se_delta", args)
        mfx$std.error <- as.numeric(se)
        J <- attr(se, "jacobian")
        draws <- NULL

    # no standard error
    } else {
        J <- draws <- NULL
    }

    # merge original data back in
    # HACK: relies on NO sorting at ANY point
    if (isTRUE(nrow(mfx) == nrow(cache$original))) {
        idx <- setdiff(colnames(cache$original), colnames(mfx))
        mfx <- data.table(mfx, cache$original[, ..idx])
    }

    # meta info
    mfx[["type"]] <- type

    mfx <- get_ci(
        mfx,
        conf_level = conf_level,
        # sometimes get_predicted fails on SE but succeeds on CI (e.g., betareg)
        df = dof,
        overwrite = FALSE,
        draws = draws,
        estimate = "comparison")


    # group id: useful for merging, only if it's an internal call and not user-initiated
    if (isTRUE(internal_call) && !"group" %in% colnames(mfx)) {
         mfx$group <- "main_marginaleffect"
    }

    if (!is.null(transform_post)) {
        mfx <- backtransform(mfx, transform_post)
    }

    # clean columns
    stubcols <- c("rowid", "rowid_counterfactual", "type", "group", "term", "hypothesis",
                  grep("^contrast", colnames(mfx), value = TRUE),
                  "comparison", "std.error", "statistic", "p.value", "conf.low", "conf.high", "df",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(mfx))
    cols <- unique(c(cols, colnames(mfx)))
    mfx <- mfx[, ..cols, drop = FALSE]
    mfx[["eps"]] <- NULL

    # save as attribute and not column
    marginaleffects_wts_internal <- mfx[["marginaleffects_wts_internal"]]
    mfx[["marginaleffects_wts_internal"]] <- NULL

    out <- mfx

    if ("eps_tmp" %in% colnames(out)) {
        out[, "eps_tmp" := NULL]
    }

    if (!isTRUE(internal_call)) {
        setDF(out)
    }

    class(out) <- c("comparisons", class(out))
    attr(out, "posterior_draws") <- draws
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables_vec
    attr(out, "jacobian") <- J
    attr(out, "vcov") <- vcov
    attr(out, "vcov.type") <- vcov.type
    attr(out, "transform_pre") <- transform_pre_label
    attr(out, "transform_post") <- transform_post_label
    attr(out, "weights") <- marginaleffects_wts_internal

    # modelbased::visualisation_matrix attaches useful info for plotting
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }

    if (!isTRUE(internal_call)) {
        if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
            out$group <- NULL
        }
    }

    return(out)
}
