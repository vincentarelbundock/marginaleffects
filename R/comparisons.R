#' Comparisons Between Predictions Made With Different Regressor Values
#'
#' @description
#' Predict the outcome variable at different regressor values (e.g., college
#' graduates vs. others), and compare those predictions by computing a difference,
#' ratio, or some other function. `comparisons()` can return many quantities of
#' interest, such as contrasts, differences, risk ratios, changes in log odds,
#' slopes, elasticities, etc.
#'
#' * `comparisons()`: unit-level (conditional) estimates.
#' * `avg_comparisons()`: average (marginal) estimates.
#'
#' `variables` identifies the focal regressors whose "effect" we are interested in. `comparison` determines how predictions with different regressor values are compared (difference, ratio, odds, etc.). The `newdata` argument and the `datagrid()` function control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.
#'
#' See the comparisons vignette and package website for worked examples and case studies:
#'
#' * <https://vincentarelbundock.github.io/marginaleffects/articles/comparisons.html>
#' * <https://vincentarelbundock.github.io/marginaleffects/>
#'
#' @inheritParams slopes
#' @inheritParams predictions
#' @param variables Focal variables
#' * `NULL`: compute comparisons for all the variables in the model object (can be slow).
#' * Character vector: subset of variables (usually faster).
#' * Named list: names identify the subset of variables of interest, and values define the type of contrast to compute. Acceptable values depend on the variable type:
#'   - Factor or character variables:
#'     * "reference": Each factor level is compared to the factor reference (base) level
#'     * "all": All combinations of observed levels
#'     * "sequential": Each factor level is compared to the previous factor level
#'     * "pairwise": Each factor level is compared to all other levels
#'     * "minmax": The highest and lowest levels of a factor.
#'     * Vector of length 2 with the two values to compare.
#'   - Logical variables:
#'     * NULL: contrast between TRUE and FALSE
#'   - Numeric variables:
#'     * Numeric of length 1: Contrast for a gap of `x`, computed at the observed value plus and minus `x / 2`. For example, estimating a `+1` contrast compares adjusted predictions when the regressor is equal to its observed value minus 0.5 and its observed value plus 0.5.
#'     * Numeric vector of length 2: Contrast between the 2nd element and the 1st element of the `x` vector.
#'     * Data frame with the same number of rows as `newdata`, with two columns of "low" and "high" values to compare.
#'     * Function which accepts a numeric vector and returns a data frame with two columns of "low" and "high" values to compare. See examples below.
#'     * "iqr": Contrast across the interquartile range of the regressor.
#'     * "sd": Contrast across one standard deviation around the regressor mean.
#'     * "2sd": Contrast across two standard deviations around the regressor mean.
#'     * "minmax": Contrast between the maximum and the minimum values of the regressor.
#'   - Examples:
#'     + `variables = list(gear = "pairwise", hp = 10)`
#'     + `variables = list(gear = "sequential", hp = c(100, 120))`
#'     + See the Examples section below for more.
#' @param newdata Grid of predictor values at which we evaluate the comparisons.
#' + `NULL` (default): Unit-level contrasts for each observed value in the original dataset (empirical distribution).
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
#' @param comparison How should pairs of predictions be compared? Difference, ratio, odds ratio, or user-defined functions.
#' * string: shortcuts to common contrast functions.
#'   - Supported shortcuts strings: `r paste(names(marginaleffects:::comparison_function_dict), collapse = ", ")`
#'   - See the Comparisons section below for definitions of each transformation.
#' * function: accept two equal-length numeric vectors of adjusted predictions (`hi` and `lo`) and returns a vector of contrasts of the same length, or a unique numeric value.
#'   - See the Transformations section below for examples of valid functions.
#' @param transform string or function. Transformation applied to unit-level estimates and confidence intervals just before the function returns results. Functions must accept a vector and return a vector of the same length. Support string shortcuts: "exp", "ln"
#' @param equivalence Numeric vector of length 2: bounds used for the two-one-sided test (TOST) of equivalence, and for the non-inferiority and non-superiority tests. See Details section below.
#' @param by Aggregate unit-level estimates (aka, marginalize, average over). Valid inputs:
#'   - `FALSE`: return the original unit-level estimates.
#'   - `TRUE`: aggregate estimates for each term.
#'   - Character vector of column names in `newdata` or in the data frame produced by calling the function without the `by` argument.
#'   - Data frame with a `by` column of group labels, and merging columns shared by `newdata` or the data frame produced by calling the same function without the `by` argument.
#'   - See examples below.
#' @param cross
#' * `FALSE`: Contrasts represent the change in adjusted predictions when one predictor changes and all other variables are held constant.
#' * `TRUE`: Contrasts represent the changes in adjusted predictions when all the predictors specified in the `variables` argument are manipulated simultaneously (a "cross-contrast").
#' @template deltamethod
#' @template model_specific_arguments
#' @template comparison_functions
#' @template bayesian
#' @template equivalence
#'
#' @return A `data.frame` with one row per observation (per term/group) and several columns:
#' * `rowid`: row number of the `newdata` data frame
#' * `type`: prediction type, as defined by the `type` argument
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `term`: the variable whose marginal effect is computed
#' * `dydx`: slope of the outcome with respect to the term, for a given combination of predictor values
#' * `std.error`: standard errors computed by via the delta method.
#' * `p.value`: p value associated to the `estimate` column. The null is determined by the `hypothesis` argument (0 by default), and p values are computed before applying the `transform` argument.
#'
#' See `?print.marginaleffects` for printing options.
#'
#' @examples
#' \dontrun{
#' library(marginaleffects)
#'
#' # Linear model
#' tmp <- mtcars
#' tmp$am <- as.logical(tmp$am)
#' mod <- lm(mpg ~ am + factor(cyl), tmp)
#' avg_comparisons(mod, variables = list(cyl = "reference"))
#' avg_comparisons(mod, variables = list(cyl = "sequential"))
#' avg_comparisons(mod, variables = list(cyl = "pairwise"))
#'
#' # GLM with different scale types
#' mod <- glm(am ~ factor(gear), data = mtcars)
#' avg_comparisons(mod, type = "response")
#' avg_comparisons(mod, type = "link")
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
#' m <- lm(mpg ~ hp + drat + factor(cyl) + factor(am), data = mtcars)
#' comparisons(m, variables = "hp", newdata = datagrid(FUN_factor = unique, FUN_numeric = median))
#'
#' # Numeric contrasts
#' mod <- lm(mpg ~ hp, data = mtcars)
#' avg_comparisons(mod, variables = list(hp = 1))
#' avg_comparisons(mod, variables = list(hp = 5))
#' avg_comparisons(mod, variables = list(hp = c(90, 100)))
#' avg_comparisons(mod, variables = list(hp = "iqr"))
#' avg_comparisons(mod, variables = list(hp = "sd"))
#' avg_comparisons(mod, variables = list(hp = "minmax"))
#'
#' # using a function to specify a custom difference in one regressor
#' dat <- mtcars
#' dat$new_hp <- 49 * (dat$hp - min(dat$hp)) / (max(dat$hp) - min(dat$hp)) + 1
#' modlog <- lm(mpg ~ log(new_hp) + factor(cyl), data = dat)
#' fdiff <- \(x) data.frame(x, x + 10)
#' avg_comparisons(modlog, variables = list(new_hp = fdiff))
#'
#' # Adjusted Risk Ratio: see the contrasts vignette
#' mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
#' avg_comparisons(mod, comparison = "lnratioavg", transform = exp)
#'
#' # Adjusted Risk Ratio: Manual specification of the `comparison`
#' avg_comparisons(
#'      mod,
#'      comparison = function(hi, lo) log(mean(hi) / mean(lo)),
#'      transform = exp)
#
#' # cross contrasts
#' mod <- lm(mpg ~ factor(cyl) * factor(gear) + hp, data = mtcars)
#' avg_comparisons(mod, variables = c("cyl", "gear"), cross = TRUE)
#'
#' # variable-specific contrasts
#' avg_comparisons(mod, variables = list(gear = "sequential", hp = 10))
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
#' comparisons(mod, by = TRUE)
#'
#' mod <- lm(mpg ~ hp * am * vs, data = mtcars)
#' avg_comparisons(mod, variables = "hp", by = c("vs", "am"))
#'
#' library(nnet)
#' mod <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
#' by <- data.frame(
#'     group = c("3", "4", "5"),
#'     by = c("3,4", "3,4", "5"))
#' comparisons(mod, type = "probs", by = by)
#' }
#'
#' @export
comparisons <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        comparison = "difference",
                        type = NULL,
                        vcov = TRUE,
                        by = FALSE,
                        conf_level = 0.95,
                        transform = NULL,
                        cross = FALSE,
                        wts = NULL,
                        hypothesis = NULL,
                        equivalence = NULL,
                        p_adjust = NULL,
                        df = Inf,
                        eps = NULL,
                        ...) {


    dots <- list(...)
    
    # backward compatibility
    if ("transform_post" %in% names(dots)) transform <- dots[["transform_post"]]
    if ("transform_pre" %in% names(dots)) comparison <- dots[["transform_pre"]]

    # required by stubcols later, but might be overwritten
    bycols <- NULL

    if (!is.null(equivalence) && !is.null(p_adjust)) {
        insight::format_error("The `equivalence` and `p_adjust` arguments cannot be used together.")
    }

    # slopes()` **must** run its own sanity checks and hardcode valid arguments
    internal_call <- dots[["internal_call"]]
    if (!isTRUE(internal_call)) {
        # if `newdata` is a call to `datagrid`, `typical`, or `counterfactual`,
        # insert `model` should probably not be nested too deeply in the call
        # stack since we eval.parent() (not sure about this)
        scall <- substitute(newdata)
        newdata <- sanitize_newdata_call(scall, newdata, model)

        model <- sanitize_model(
            model = model,
            newdata = newdata,
            wts = wts,
            vcov = vcov,
            calling_function = "comparisons",
            ...)
        cross <- sanitize_cross(cross, variables, model)
        type <- sanitize_type(model = model, type = type)

    # internal call from `slopes()`
    } else {
        # not allowed in `slopes()`
        cross <- FALSE
    }

    # build call: match.call() doesn't work well in *apply()
    call_attr <- c(list(
        name = "comparisons",
        model = model,
        newdata = newdata,
        variables = variables,
        type = type,
        vcov = vcov,
        by = by,
        conf_level = conf_level,
        comparison = comparison,
        transform = transform,
        cross = cross,
        wts = wts,
        hypothesis = hypothesis,
        df = df),
        list(...))
    call_attr <- do.call("call", call_attr)
    
    # multiple imputation
    if (inherits(model, "mira")) {
        out <- process_imputation(model, call_attr)
        return(out)
    }
    
    # more sanity chekcs
    
    conf_level <- sanitize_conf_level(conf_level, ...)
    sanity_dots(model, ...)
    checkmate::assert_number(eps, lower = 1e-10, null.ok = TRUE)
    checkmate::assert_number(df, lower = 1)

    # transforms
    sanity_comparison(comparison)
    comparison_label <- transform_label <- NULL
    if (is.function(comparison)) {
        comparison_label <- deparse(substitute(comparison))
    }
    if (is.function(transform)) {
        transform_label <- deparse(substitute(transform))
        transform <- sanitize_transform(transform)
        names(transform) <- transform_label
    } else {
        transform <- sanitize_transform(transform)
        transform_label <- names(transform)
    }

    marginalmeans <- isTRUE(checkmate::check_choice(newdata, choices = "marginalmeans"))

    # before sanitize_variables
    if (isTRUE(checkmate::check_character(by))) {
        addvar <- by
    } else if (isTRUE(checkmate::check_data_frame(by))) {
        addvar <- colnames(by)
    } else {
        addvar <- FALSE
    }
    
    # extracting modeldata repeatedly is slow.
    # checking dots allows cheap multiple imputation
    dots <- list(...)
    if ("modeldata" %in% names(dots)) {
        modeldata <- dots[["modeldata"]]
    } else {
        if (isTRUE(checkmate::check_character(by))) {
            addvar <- by
        } else if (isTRUE(checkmate::check_data_frame(by))) {
            addvar <- colnames(by)
        } else {
            addvar <- FALSE
        }
        modeldata <- get_modeldata(model, additional_variables = addvar)
    }
    
    newdata <- sanitize_newdata(
        model = model,
        newdata = newdata,
        by = by,
        modeldata = modeldata)
    
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
        modeldata = modeldata,
        variables = variables,
        cross = cross,
        by = by,
        comparison = comparison,
        eps = eps)

    tmp <- sanitize_hypothesis(hypothesis, ...)
    hypothesis <- tmp$hypothesis
    hypothesis_null <- tmp$hypothesis_null

    # after sanitize_newdata
    sanity_by(by, newdata)

    # after sanity_by
    newdata <- dedup_newdata(
        model = model,
        newdata = newdata,
        wts = wts,
        by = by,
        cross = cross,
        comparison = comparison)
    if (is.null(wts) && "marginaleffects_wts_internal" %in% colnames(newdata)) {
        wts <- "marginaleffects_wts_internal"
    }

    # get dof before transforming the vcov arg
    # get_df() produces a weird warning on non lmerMod. We can skip them
    # because get_vcov() will produce an informative error later.
    if (inherits(model, "lmerMod") && (isTRUE(hush(vcov %in% c("satterthwaite", "kenward-roger"))))) {
        # predict.lmerTest requires the DV
        dv <- insight::find_response(model)
        if (!dv %in% colnames(newdata)) {
            newdata[[dv]] <- mean(insight::get_response(model))
        }

        if (!isTRUE(hush(is.infinite(df)))) {
            insight::format_error('The `df` argument is not supported when `vcov` is "satterthwaite" or "kenward-roger".')
        }

        # df_per_observation is an undocumented argument introduced in 0.18.4.7 to preserve backward incompatibility
        df <- insight::get_df(model, type = vcov, data = newdata, df_per_observation = TRUE)
    }

    vcov_false <- isFALSE(vcov)
    vcov.type <- get_vcov_label(vcov)
    vcov <- get_vcov(model, vcov = vcov, ...)

    predictors <- variables_list$conditional

    ############### sanity checks are over

    # Bootstrap
    out <- inferences_dispatch(
        FUN = comparisons,
        model = model, newdata = newdata, vcov = vcov, variables = variables, type = type, by = by,
        conf_level = conf_level,
        comparison = comparison, transform = transform, wts = wts, hypothesis = hypothesis, eps = eps, ...)
    if (!is.null(out)) {
        return(out)
    }


    # compute contrasts and standard errors
    args <- list(model = model,
                 newdata = newdata,
                 variables = predictors,
                 cross = cross,
                 marginalmeans = marginalmeans,
                 modeldata = modeldata)
    dots[["modeldata"]] <- NULL # dont' pass twice
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
                 cross = cross,
                 hypothesis = hypothesis,
                 modeldata = modeldata)
    args <- c(args, dots)
    mfx <- do.call("get_contrasts", args)

    # bayesian posterior
    if (!is.null(attr(mfx, "posterior_draws"))) {
        draws <- attr(mfx, "posterior_draws")
        J <- NULL

    # standard errors via delta method
    } else if (!vcov_false && isTRUE(checkmate::check_matrix(vcov))) {
        idx <- intersect(colnames(mfx), c("group", "term", "contrast"))
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
                     eps = eps,
                     cross = cross)
        args <- c(args, dots)
        se <- do.call("get_se_delta", args)
        J <- attr(se, "jacobian")
        attr(se, "jacobian") <- NULL
        mfx$std.error <- as.numeric(se)
        draws <- NULL

    # no standard error
    } else {
        J <- draws <- NULL
    }

    # merge original data back in
    if ((is.null(by) || isFALSE(by)) && "rowid" %in% colnames(mfx)) {
        if ("rowid" %in% colnames(newdata)) {
            idx <- c("rowid", "rowidcf", "term", "contrast", "by", setdiff(colnames(contrast_data$original), colnames(mfx)))
            idx <- intersect(idx, colnames(contrast_data$original))
            tmp <- contrast_data$original[, ..idx, drop = FALSE]
            # contrast_data is duplicated to compute contrasts for different terms or pairs
            bycols <- intersect(colnames(tmp), colnames(mfx))
            idx <- duplicated(tmp, by = bycols)
            tmp <- tmp[!idx]
            mfx <- merge(mfx, tmp, all.x = TRUE, by = bycols, sort = FALSE)
        # HACK: relies on NO sorting at ANY point
        } else {
            idx <- setdiff(colnames(contrast_data$original), colnames(mfx))
            mfx <- data.table(mfx, contrast_data$original[, ..idx])
        }
    }

    # meta info
    mfx <- get_ci(
        mfx,
        conf_level = conf_level,
        df = df,
        draws = draws,
        estimate = "estimate",
        null_hypothesis = hypothesis_null,
        p_adjust = p_adjust,
        model = model)


    # group id: useful for merging, only if it's an internal call and not user-initiated
    if (isTRUE(internal_call) && !"group" %in% colnames(mfx)) {
         mfx$group <- "main_marginaleffect"
    }


    # clean rows and columns
    if (is.null(bycols) && isTRUE(checkmate::check_character(by))) {
        bycols <- by
    }

    if (is.null(draws)) {
        idx <- c("term", grep("^term$|^contrast$|^contrast_|^rowid|^group$", colnames(mfx), value = TRUE), bycols)
        idx <- intersect(idx, colnames(mfx))
        if (length(idx) > 0) data.table::setorderv(mfx, cols = idx)
    }

    stubcols <- c(
        "rowid", "rowidcf", "group", "term", "hypothesis", "by",
        grep("^contrast", colnames(mfx), value = TRUE),
        bycols,
        "estimate", "std.error", "statistic", "p.value", "conf.low",
        "conf.high", "df", "predicted", "predicted_hi", "predicted_lo")
    cols <- intersect(stubcols, colnames(mfx))
    cols <- unique(c(cols, colnames(mfx)))
    if (length(setdiff(names(mfx), cols)) > 0L) {
      mfx[, setdiff(names(mfx), cols) := NULL]
    }
    mfx <- sort_columns(mfx, stubcols)


    # bayesian draws
    attr(mfx, "posterior_draws") <- draws

    # equivalence tests
    mfx <- equivalence(mfx, equivalence = equivalence, df = df, ...)

    # after draws attribute
    mfx <- backtransform(mfx, transform)

    # save as attribute and not column
    if (any(!is.na(mfx[["marginaleffects_wts_internal"]]))) {
        marginaleffects_wts_internal <- mfx[["marginaleffects_wts_internal"]]
    } else {
        marginaleffects_wts_internal <- NULL
    }
    mfx[["marginaleffects_wts_internal"]] <- NULL

    out <- mfx

    if (!isTRUE(internal_call)) {
        data.table::setDF(out)
    }

    out <- set_marginaleffects_attributes(
        out,
        get_marginaleffects_attributes(newdata, include_regex = "^newdata.*class|explicit|matrix|levels"))


    # save newdata and model for use in recall()
    if (!"newdata" %in% names(call_attr) || is.null(call_attr[["newdata"]])) {
        attr(out, "newdata") <- newdata
    }

    # other attributes
    attr(out, "call") <- call_attr
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "model") <- model
    attr(out, "variables") <- predictors
    attr(out, "jacobian") <- J
    attr(out, "vcov") <- vcov
    attr(out, "vcov.type") <- vcov.type
    attr(out, "weights") <- marginaleffects_wts_internal
    attr(out, "comparison") <- comparison
    attr(out, "transform") <- transform[[1]]
    attr(out, "comparison_label") <- comparison_label
    attr(out, "transform_label") <- transform_label
    attr(out, "conf_level") <- conf_level
    attr(out, "by") <- by

    if (inherits(model, "brmsfit")) {
        insight::check_if_installed("brms")
        attr(out, "nchains") <- brms::nchains(model)
    }

    if (!isTRUE(internal_call)) {
        if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
            out$group <- NULL
        }
    }

    class(out) <- c("comparisons", class(out))
    return(out)
}


#' Average comparisons
#' @describeIn comparisons Average comparisons
#' @export
#'
avg_comparisons <- function(model,
                            newdata = NULL,
                            variables = NULL,
                            type = NULL,
                            vcov = TRUE,
                            by = TRUE,
                            conf_level = 0.95,
                            comparison = "difference",
                            transform = NULL,
                            cross = FALSE,
                            wts = NULL,
                            hypothesis = NULL,
                            equivalence = NULL,
                            p_adjust = NULL,
                            df = Inf,
                            eps = NULL,
                            ...) {

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    scall <- substitute(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, model)

    # Bootstrap
    out <- inferences_dispatch(
        FUN = avg_comparisons,
        model = model, newdata = newdata, vcov = vcov, variables = variables, type = type, by = by,
        conf_level = conf_level,
        comparison = comparison, transform = transform, wts = wts, hypothesis = hypothesis, eps = eps, ...)
    if (!is.null(out)) {
        return(out)
    }

    out <- comparisons(
        model = model,
        newdata = newdata,
        variables = variables,
        type = type,
        vcov = vcov,
        by = by,
        conf_level = conf_level,
        comparison = comparison,
        transform = transform,
        cross = cross,
        wts = wts,
        hypothesis = hypothesis,
        equivalence = equivalence,
        p_adjust = p_adjust,
        df = df,
        eps = eps,
        ...)

    return(out)
}
