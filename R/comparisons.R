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
#'     * Numeric of length 1: Contrast for a gap of `x`, computed at the observed value plus and minus `x / 2`. For example, estimating a `+1` contrast compares adjusted predictions when the regressor is equal to its observed value minus 0.5 and its observed value plus 0.5.
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
#'   - "tukey": Contrasts at Tukey's 5 numbers.
#'   - "grid": Contrasts on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - `newdata = datagrid(mpg = fivenum)`: `mpg` variable held at Tukey's five numbers (using the `fivenum` function), and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid] documentation.
#' @param transform_pre string or function. How should pairs of adjusted predictions be contrasted?
#' * string: shortcuts to common contrast functions.
#'   - Supported shortcuts strings: `r paste(names(marginaleffects:::transform_pre_function_dict), collapse = ", ")`
#'   - See the Transformations section below for definitions of each transformation.
#' * function: accept two equal-length numeric vectors of adjusted predictions (`hi` and `lo`) and returns a vector of contrasts of the same length, or a unique numeric value.
#'   - See the Transformations section below for examples of valid functions.
#' @param transform_post (experimental) A function applied to unit-level estimates and confidence intervals just before the function returns results.
#' @param by Compute group-wise average estimates. Valid inputs:
#'   - Character vector of column names in `newdata` or in the data frame produced by calling the function without the `by` argument.
#'   - Data frame with a `by` column of group labels, and merging columns shared by `newdata` or the data frame produced by calling the same function without the `by` argument.
#'   - See examples below.
#' @param interaction TRUE, FALSE, or NULL
#' * `FALSE`: Contrasts represent the change in adjusted predictions when one predictor changes and all other variables are held constant.
#' * `TRUE`: Contrasts represent the changes in adjusted predictions when the predictors specified in the `variables` argument are manipulated simultaneously.
#' * `NULL` (default): Behaves like `TRUE` when the `variables` argument is specified and the model formula includes interactions. Behaves like `FALSE` otherwise.
#' @template model_specific_arguments
#' @template transform_pre_functions
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
#' comparisons(mod, newdata = datagrid(am = 0, gear = tmp$gear))
#' comparisons(mod, newdata = datagrid(am = unique, gear = max))
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
#' summary(cmp, transform_avg = exp)
#'
#' # Adjusted Risk Ratio: Manual specification of the `transform_pre`
#' cmp <- comparisons(mod, transform_pre = function(hi, lo) log(mean(hi) / mean(lo)))
#' summary(cmp, transform_avg = exp)
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
#' 
#' # `by` argument
#' mod <- lm(mpg ~ hp * am * vs, data = mtcars)
#' cmp <- comparisons(mod, variables = "hp", by = c("vs", "am"))
#' summary(cmp)
#' 
#' library(nnet)
#' mod <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
#' by <- data.frame(
#'     group = c("3", "4", "5"),
#'     by = c("3,4", "3,4", "5"))
#' comparisons(mod, type = "probs", by = by)
#' 
#' @export
comparisons <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        type = NULL,
                        vcov = TRUE,
                        conf_level = 0.95,
                        transform_pre = "difference",
                        transform_post = NULL,
                        interaction = NULL,
                        by = NULL,
                        wts = NULL,
                        hypothesis = NULL,
                        eps = NULL,
                        ...) {

    dots <- list(...)

    transform_pre_label <- transform_post_label <- NULL
    if (is.function(transform_pre)) {
        transform_pre_label <- deparse(substitute(transform_pre))
    }
    if (is.function(transform_post)) {
        transform_post_label <- deparse(substitute(transform_post))
    }

    # marginaleffects()` **must** run its own sanity checks and hardcode valid arguments
    internal_call <- dots[["internal_call"]]
    if (!isTRUE(internal_call)) {
        # if `newdata` is a call to `datagrid`, `typical`, or `counterfactual`,
        # insert `model` should probably not be nested too deeply in the call
        # stack since we eval.parent() (not sure about this)
        scall <- substitute(newdata)
        if (is.call(scall) && as.character(scall)[1] %in% c("datagrid", "datagridcf", "typical", "counterfactual")) {
            lcall <- as.list(scall)
            if (!any(c("model", "data") %in% names(lcall))) {
                lcall <- c(lcall, list("model" = model))
                newdata <- eval.parent(as.call(lcall))
            }

        }

        model <- sanitize_model(
            model = model,
            newdata = newdata,
            wts = wts,
            vcov = vcov,
            calling_function = "comparisons",
            ...)
        interaction <- sanitize_interaction(interaction, variables, model)
        type <- sanitize_type(model = model, type = type, calling_function = "marginaleffects")
        checkmate::assert_function(transform_post, null.ok = TRUE)

    # internal call from `marginaleffects()`
    } else {
        # not allowed in `marginaleffects()`
        interaction <- FALSE
    }

    # bayesian models do not support `by` and "avg" in `transform_pre`
    if (!is.null(by)) {
        if (isTRUE(insight::model_info(model)$is_bayesian)) {
            msg <- format_msg(
            "The `by` argument of the `comparisons()` and `marginaleffects()` functions is not supported for bayesian models. Users can call the `posteriordraws()` function and compute the quantities manually.")
            stop(msg, call. = FALSE)
        }
    }

    conf_level <- sanitize_conf_level(conf_level, ...)
    sanity_transform_pre(transform_pre)
    checkmate::assert_numeric(eps, len = 1, lower = 1e-10, null.ok = TRUE)

    # used by `marginaleffects` to hard-code preference 
    # deprecated as user-level arguments
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

    marginalmeans <- isTRUE(checkmate::check_choice(newdata, choices = "marginalmeans")) 

    # before sanitize_variables
    newdata <- sanitize_newdata(model = model, newdata = newdata, by = by)

    # weights: before sanitize_variables
    sanity_wts(wts, newdata) # after sanity_newdata
    if (!is.null(wts) && isTRUE(checkmate::check_string(wts))) {
        newdata[["marginaleffects_wts_internal"]] <- newdata[[wts]]
    } else {
        newdata[["marginaleffects_wts_internal"]] <- wts
    }

    # after sanitize_newdata
    variables_list <- sanitize_variables(
        model = model,
        newdata = newdata,
        variables = variables,
        interaction = interaction,
        by = by,
        contrast_numeric = contrast_numeric,
        contrast_factor = contrast_factor,
        transform_pre = transform_pre)

    hypothesis <- sanitize_hypothesis(hypothesis, ...)

    # after sanitize_newdata
    sanity_by(by, newdata)


    # get dof before transforming the vcov arg
    if (is.character(vcov) &&
       # get_df() produces a weird warning on non lmerMod. We can skip them
       # because get_vcov() will produce an informative error later.
       inherits(model, "lmerMod") && 
       (isTRUE(vcov == "satterthwaite") || isTRUE(vcov == "kenward-roger"))) {
        df <- insight::find_response(model)
        # predict.lmerTest requires the DV
        if (!df %in% colnames(newdata)) {
            newdata[[df]] <- mean(insight::get_response(model))
        }
        dof <- insight::get_df(model, type = vcov, data = newdata)
    } else {
        dof <- NULL
    }

    vcov_false <- isTRUE(vcov == FALSE)
    vcov.type <- get_vcov_label(vcov)
    vcov <- get_vcov(model, vcov = vcov, ...)

    predictors <- variables_list$conditional

    # compute contrasts and standard errors
    args <- list(model = model,
                 newdata = newdata,
                 variables = predictors,
                 interaction = interaction,
                 marginalmeans = marginalmeans,
                 eps = eps)
    args <- c(args, dots)
    contrast_data <- do.call("get_contrast_data", args)

    args <- list(model,
                 newdata = newdata,
                 variables = predictors,
                 type = type,
                 original = contrast_data[["original"]],
                 hi = contrast_data[["hi"]],
                 lo = contrast_data[["lo"]],
                 wts = contrast_data[["marginaleffects_wts_internal"]],
                 by = by,
                 marginalmeans = marginalmeans,
                 interaction = interaction,
                 hypothesis = hypothesis)
    args <- c(args, dots)
    mfx <- do.call("get_contrasts", args)

    # bayesian posterior
    if (!is.null(attr(mfx, "posterior_draws"))) {
        draws <- attr(mfx, "posterior_draws")
        J <- NULL

    # standard errors via delta method
    } else if (!vcov_false && isTRUE(checkmate::check_matrix(vcov))) {
        idx <- intersect(colnames(mfx), c("type", "group", "term", "contrast"))
        idx <- mfx[, (idx), drop = FALSE]
        args <- list(model,
                     vcov = vcov,
                     type = type,
                     FUN = get_se_delta_contrasts,
                     newdata = newdata,
                     index = idx,
                     variables = predictors,
                     marginalmeans = marginalmeans,
                     hypothesis = hypothesis,
                     hi = contrast_data$hi,
                     lo = contrast_data$lo,
                     original = contrast_data$original,
                     by = by,
                     interaction = interaction,
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
    if (is.null(by) && "rowid" %in% colnames(mfx)) {
        idx <- setdiff(colnames(contrast_data$original), colnames(mfx))
        mfx <- data.table(mfx, contrast_data$original[, ..idx])
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


    # clean columns
    stubcols <- c("rowid", "rowidcf", "type", "group", "term", "hypothesis",
                  grep("^contrast", colnames(mfx), value = TRUE),
                  "comparison", "std.error", "statistic", "p.value", "conf.low", "conf.high", "df",
                  "predicted", "predicted_hi", "predicted_lo")
    cols <- intersect(stubcols, colnames(mfx))
    cols <- unique(c(cols, colnames(mfx)))
    mfx <- mfx[, ..cols, drop = FALSE]

    # bayesian draws
    attr(mfx, "posterior_draws") <- draws

    # after draws attribute
    if (!is.null(transform_post)) {
        mfx <- backtransform(mfx, transform_post)
    }

    # save as attribute and not column
    if (any(!is.na(mfx[["marginaleffects_wts_internal"]]))) {
        marginaleffects_wts_internal <- mfx[["marginaleffects_wts_internal"]]
    } else {
        marginaleffects_wts_internal <- NULL
    }
    mfx[["marginaleffects_wts_internal"]] <- NULL

    out <- mfx

    if ("marginaleffects_eps" %in% colnames(out)) {
        out[, "marginaleffects_eps" := NULL]
    }

    if (!isTRUE(internal_call)) {
        setDF(out)
    }

    out <- set_attributes(
        out,
        get_attributes(newdata, include_regex = "^newdata"))
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- predictors
    attr(out, "jacobian") <- J
    attr(out, "vcov") <- vcov
    attr(out, "vcov.type") <- vcov.type
    attr(out, "weights") <- marginaleffects_wts_internal
    attr(out, "transform_pre") <- transform_pre
    attr(out, "transform_post") <- transform_post
    attr(out, "transform_pre_label") <- transform_pre_label
    attr(out, "transform_post_label") <- transform_post_label
    attr(out, "by") <- by

    if (!isTRUE(internal_call)) {
        if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
            out$group <- NULL
        }
    }

    class(out) <- c("comparisons", class(out))
    return(out)
}
