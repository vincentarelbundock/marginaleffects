#' Comparisons Between Predictions Made With Different Regressor Values
#'
#' @description
#' Predict the outcome variable at different regressor values (e.g., college
#' graduates vs. others), and compare those predictions by computing a difference,
#' ratio, or some other function. `comparisons()` can return many quantities of
#' interest, such as contrasts, differences, risk ratios, changes in log odds, lift,
#' slopes, elasticities, etc.
#'
#' * `comparisons()`: unit-level (conditional) estimates.
#' * `avg_comparisons()`: average (marginal) estimates.
#'
#' `variables` identifies the focal regressors whose "effect" we are interested in. `comparison` determines how predictions with different regressor values are compared (difference, ratio, odds, etc.). The `newdata` argument and the `datagrid()` function control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.
#'
#' See the comparisons chapter on the package website for worked examples and case studies:
#'
#' * [https://marginaleffects.com/chapters/comparisons.html](https://marginaleffects.com/chapters/comparisons.html)
#' * [https://marginaleffects.com/](https://marginaleffects.com/)
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
#'     * "revpairwise", "revreference", "revsequential": inverse of the corresponding hypotheses.
#'     * Vector of length 2 with the two values to compare.
#'     * Data frame with the same number of rows as `newdata`, with two columns of "lo" and "hi" values to compare.
#'     * Function that accepts a vector and returns a data frame with two columns of "lo" and "hi" values to compare. See examples below.
#'   - Logical variables:
#'     * NULL: contrast between TRUE and FALSE
#'     * Data frame with the same number of rows as `newdata`, with two columns of "lo" and "hi" values to compare.
#'     * Function that accepts a vector and returns a data frame with two columns of "lo" and "hi" values to compare. See examples below.
#'   - Numeric variables:
#'     * Numeric of length 1: Forward contrast for a gap of `x`, computed between the observed value and the observed value plus `x`. Users can set a global option to get a "center" or "backward" contrast instead: `options(marginaleffects_contrast_direction="center")`
#'     * Numeric vector of length 2: Contrast between the largest and the smallest elements of the `x` vector.
#'     * Data frame with the same number of rows as `newdata`, with two columns of "lo" and "hi" values to compare.
#'     * Function that accepts a vector and returns a data frame with two columns of "lo" and "hi" values to compare. See examples below.
#'     * "iqr": Contrast across the interquartile range of the regressor.
#'     * "sd": Contrast across one standard deviation around the regressor mean.
#'     * "2sd": Contrast across two standard deviations around the regressor mean.
#'     * "minmax": Contrast between the maximum and the minimum values of the regressor.
#'   - Examples:
#'     + `variables = list(gear = "pairwise", hp = 10)`
#'     + `variables = list(gear = "sequential", hp = c(100, 120))`
#'     + `variables = list(hp = function(x) data.frame(low = x - 5, high = x + 10))`
#'     + See the Examples section below for more.
#' @param newdata Grid of predictor values at which we evaluate the comparisons.
#' + Warning: Avoid modifying your dataset between fitting the model and calling a `marginaleffects` function. This can sometimes lead to unexpected results.
#' + `NULL` (default): Unit-level contrasts for each observed value in the dataset (empirical distribution). The dataset is retrieved using [insight::get_data()], which tries to extract data from the environment. This may produce unexpected results if the original data frame has been altered since fitting the model.
#' + data frame: Unit-level contrasts for each row of the `newdata` data frame.
#' + string:
#'   - "mean": Contrasts at the Mean. Contrasts when each predictor is held at its mean or mode.
#'   - "median": Contrasts at the Median. Contrasts when each predictor is held at its median or mode.
#'   - "balanced": Contrasts evaluated on a balanced grid with every combination of categories and numeric variables held at their means.
#'   - "tukey": Contrasts at Tukey's 5 numbers.
#'   - "grid": Contrasts on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - `newdata = datagrid(mpg = fivenum)`: `mpg` variable held at Tukey's five numbers (using the `fivenum` function), and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid] documentation.
#' + [subset()] call with a single argument to select a subset of the dataset used to fit the model, ex: `newdata = subset(treatment == 1)`
#' + [dplyr::filter()] call with a single argument to select a subset of the dataset used to fit the model, ex: `newdata = filter(treatment == 1)`
#' @param comparison How should pairs of predictions be compared? Difference, ratio, odds ratio, or user-defined functions.
#' * string: shortcuts to common contrast functions.
#'   - Supported shortcuts strings: `r toString(names(marginaleffects:::comparison_function_dict))`
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
#'   - For more complex aggregations, you can use the `FUN` argument of the `hypotheses()` function. See that function's documentation and the Hypothesis Test vignettes on the `marginaleffects` website.
#' @param cross
#' * `FALSE`: Contrasts represent the change in adjusted predictions when one predictor changes and all other variables are held constant.
#' * `TRUE`: Contrasts represent the changes in adjusted predictions when all the predictors specified in the `variables` argument are manipulated simultaneously (a "cross-contrast").
#' @template references
#' @template deltamethod
#' @template model_specific_arguments
#' @template comparison_functions
#' @template bayesian
#' @template equivalence
#' @template type
#' @template order_of_operations
#' @template parallel
#' @template options
#' @template return
#'
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
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
#' comparisons(mod, newdata = "balanced")
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
#' fdiff <- function(x) data.frame(x, x + 10)
#' avg_comparisons(modlog, variables = list(new_hp = fdiff))
#'
#' # Adjusted Risk Ratio
#' mod <- glm(vs ~ mpg, data = mtcars, family = binomial)
#' avg_comparisons(mod, comparison = "lnratioavg", transform = exp)
#'
#' # Adjusted Risk Ratio: Manual specification of the `comparison`
#' avg_comparisons(
#'   mod,
#'   comparison = function(hi, lo) log(mean(hi) / mean(lo)),
#'   transform = exp)
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
#'   mod,
#'   newdata = "mean",
#'   hypothesis = "wt = drat")
#'
#' # same hypothesis test using row indices
#' comparisons(
#'   mod,
#'   newdata = "mean",
#'   hypothesis = "b1 - b2 = 0")
#'
#' # same hypothesis test using numeric vector of weights
#' comparisons(
#'   mod,
#'   newdata = "mean",
#'   hypothesis = c(1, -1))
#'
#' # two custom contrasts using a matrix of weights
#' lc <- matrix(
#'   c(
#'     1, -1,
#'     2, 3),
#'   ncol = 2)
#' comparisons(
#'   mod,
#'   newdata = "mean",
#'   hypothesis = lc)
#'
#' # Effect of a 1 group-wise standard deviation change
#' # First we calculate the SD in each group of `cyl`
#' # Second, we use that SD as the treatment size in the `variables` argument
#' library(dplyr)
#' mod <- lm(mpg ~ hp + factor(cyl), mtcars)
#' tmp <- mtcars %>%
#'   group_by(cyl) %>%
#'   mutate(hp_sd = sd(hp))
#' avg_comparisons(mod,
#'   variables = list(hp = function(x) data.frame(x, x + tmp$hp_sd)),
#'   by = "cyl")
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
#'   group = c("3", "4", "5"),
#'   by = c("3,4", "3,4", "5"))
#' comparisons(mod, type = "probs", by = by)
#'
#' @export
comparisons <- function(
    model,
    newdata = NULL,
    variables = NULL,
    comparison = "difference",
    type = NULL,
    vcov = TRUE,
    by = FALSE,
    conf_level = 0.95,
    transform = NULL,
    cross = FALSE,
    wts = FALSE,
    hypothesis = NULL,
    equivalence = NULL,
    df = Inf,
    eps = NULL,
    numderiv = "fdforward",
    ...
) {
    call_attr <- construct_call(model, "comparisons")

    methods <- c("rsample", "boot", "fwb", "simulation")
    if (isTRUE(checkmate::check_choice(vcov, methods))) {
        inferences_method <- vcov
        vcov <- FALSE
    } else {
        inferences_method <- NULL
    }

    # multiple imputation
    if (inherits(model, c("mira", "amest"))) {
        out <- process_imputation(model, call_attr)
        return(out)
    }

    dots <- list(...)

    # extracting modeldata repeatedly is slow.
    if ("modeldata" %in% ...names()) {
        modeldata <- call_attr[["modeldata"]] <- ...get("modeldata")
    } else {
        modeldata <- get_modeldata(
            model,
            additional_variables = if (is.logical(by)) FALSE else TRUE,
            modeldata = dots[["modeldata"]],
            wts = wts
        )
        if (isTRUE(checkmate::check_data_frame(modeldata))) {
            call_attr[["modeldata"]] <- modeldata
        }
    }

    # very early, before any use of newdata
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    scall <- rlang::enquo(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, model, by = by)

    # required by stubcols later, but might be overwritten
    bycols <- NULL

    # sanity checks
    sanity_dots(model, ...)
    conf_level <- sanitize_conf_level(conf_level, ...)
    checkmate::assert_number(eps, lower = 1e-10, null.ok = TRUE)
    numderiv <- sanitize_numderiv(numderiv)
    model <- sanitize_model(
        model = model,
        newdata = newdata,
        wts = wts,
        vcov = vcov,
        by = by,
        calling_function = "comparisons",
        ...
    )
    df <- sanitize_df(
        df = df,
        model = model,
        newdata = newdata,
        vcov = vcov,
        by = by,
        hypothesis = hypothesis
    )
    cross <- sanitize_cross(cross, variables, model)
    type <- sanitize_type(
        model = model,
        type = type,
        calling_function = "comparisons"
    )
    sanity_comparison(comparison)

    # transforms
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

    newdata <- sanitize_newdata(
        model = model,
        newdata = newdata,
        modeldata = modeldata,
        by = by,
        wts = wts
    )

    # after sanitize_newdata
    sanity_by(by, newdata)

    # after sanity_by
    newdata <- dedup_newdata(
        model = model,
        newdata = newdata,
        wts = wts,
        by = by,
        cross = cross,
        comparison = comparison
    )
    if (isFALSE(wts) && "marginaleffects_wts_internal" %in% colnames(newdata)) {
        wts <- "marginaleffects_wts_internal"
    }

    # after sanitize_newdata
    # after dedup_newdata
    variables_list <- sanitize_variables(
        model = model,
        newdata = newdata,
        modeldata = modeldata,
        variables = variables,
        cross = cross,
        by = by,
        comparison = comparison,
        eps = eps
    )

    # get dof before transforming the vcov arg
    # get_degrees_of_freedom() produces a weird warning on non lmerMod. We can skip them
    # because get_vcov() will produce an informative error later.
    df <- get_degrees_of_freedom(model = model, df = df, newdata = newdata)

    vcov_false <- isFALSE(vcov)
    vcov.type <- get_vcov_label(vcov)
    vcov <- get_vcov(model, vcov = vcov, type = type, ...)

    predictors <- variables_list$conditional

    ############### sanity checks are over

    # after inferences dispatch
    tmp <- sanitize_hypothesis(hypothesis, ...)
    hypothesis <- tmp$hypothesis
    hypothesis_null <- tmp$hypothesis_null
    hypothesis_direction <- tmp$hypothesis_direction

    # compute contrasts and standard errors
    args <- list(
        model = model,
        newdata = newdata,
        variables = predictors,
        cross = cross,
        modeldata = modeldata
    )
    dots[["modeldata"]] <- NULL # dont' pass twice
    args <- utils::modifyList(args, dots)
    contrast_data <- do.call("get_contrast_data", args)

    args <- list(
        model,
        newdata = newdata,
        variables = predictors,
        type = type,
        original = contrast_data[["original"]],
        hi = contrast_data[["hi"]],
        lo = contrast_data[["lo"]],
        wts = contrast_data[["original"]][["marginaleffects_wts_internal"]],
        by = by,
        cross = cross,
        hypothesis = hypothesis,
        modeldata = modeldata
    )
    args <- utils::modifyList(args, dots)
    mfx <- do.call("get_contrasts", args)

    hyp_by <- attr(mfx, "hypothesis_function_by")

    # bayesian posterior
    if (!is.null(attr(mfx, "posterior_draws"))) {
        draws <- attr(mfx, "posterior_draws")
        J <- NULL

        # standard errors via delta method
    } else if (!vcov_false && isTRUE(checkmate::check_matrix(vcov))) {
        idx <- intersect(colnames(mfx), c("group", "term", "contrast"))
        idx <- mfx[, (idx), drop = FALSE]
        args <- list(
            model,
            vcov = vcov,
            type = type,
            FUN = get_se_delta_contrasts,
            newdata = newdata,
            index = idx,
            variables = predictors,
            hypothesis = hypothesis,
            hi = contrast_data$hi,
            lo = contrast_data$lo,
            original = contrast_data$original,
            by = by,
            eps = eps,
            cross = cross,
            numderiv = numderiv
        )
        args <- utils::modifyList(args, dots)
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
            idx <- c(
                "rowid",
                "rowidcf",
                "term",
                "contrast",
                "by",
                setdiff(colnames(contrast_data$original), colnames(mfx))
            )
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
        hypothesis_null = hypothesis_null,
        hypothesis_direction = hypothesis_direction,
        model = model
    )

    # clean rows and columns
    # WARNING: we cannot sort rows at the end because `get_hypothesis()` is
    # applied in the middle, and it must already be sorted in the final order,
    # otherwise, users cannot know for sure what is going to be the first and
    # second rows, etc.
    mfx <- sort_columns(mfx, newdata, by)

    # bayesian draws
    attr(mfx, "posterior_draws") <- draws

    # equivalence tests
    mfx <- equivalence(mfx, equivalence = equivalence, df = df, ...)

    # after draws attribute
    mfx <- backtransform(mfx, transform)

    # save as attribute and not column
    if (!all(is.na(mfx[["marginaleffects_wts_internal"]]))) {
        marginaleffects_wts_internal <- mfx[["marginaleffects_wts_internal"]]
    } else {
        marginaleffects_wts_internal <- NULL
    }
    mfx[["marginaleffects_wts_internal"]] <- NULL

    out <- mfx

    data.table::setDF(out)

    out <- set_marginaleffects_attributes(
        out,
        get_marginaleffects_attributes(
            newdata,
            include_regex = "^newdata.*class|explicit|matrix|levels"
        )
    )

    # Global option for lean return object
    lean <- getOption("marginaleffects_lean", default = FALSE)

    # Only add (potentially large) attributes if lean is FALSE
    # extra attributes needed for print method, even with lean return object
    attr(out, "conf_level") <- conf_level
    attr(out, "by") <- by
    attr(out, "lean") <- lean
    attr(out, "vcov.type") <- vcov.type
    if (isTRUE(lean)) {
        for (a in setdiff(names(attributes(out)), c("names", "row.names", "class"))) attr(out, a) <- NULL
    } else {
        # other attributes
        attr(out, "newdata") <- newdata
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
        attr(out, "hypothesis_by") <- hyp_by
        attr(out, "transform_label") <- transform_label

        if (inherits(model, "brmsfit")) {
            insight::check_if_installed("brms")
            attr(out, "nchains") <- brms::nchains(model)
        }
    }

    class(out) <- c("comparisons", class(out))

    if (!is.null(inferences_method)) {
        out <- inferences(out, method = inferences_method)
    }

    return(out)
}


#' Average comparisons
#' @describeIn comparisons Average comparisons
#' @export
#'
avg_comparisons <- function(
    model,
    newdata = NULL,
    variables = NULL,
    type = NULL,
    vcov = TRUE,
    by = TRUE,
    conf_level = 0.95,
    comparison = "difference",
    transform = NULL,
    cross = FALSE,
    wts = FALSE,
    hypothesis = NULL,
    equivalence = NULL,
    df = Inf,
    eps = NULL,
    numderiv = "fdforward",
    ...
) {
    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    # scall <- rlang::enquo(newdata)
    # newdata <- sanitize_newdata_call(scall, newdata, model, by = by)

    #Construct comparisons() call
    call_attr <- construct_call(model, "comparisons")

    out <- evalup(call_attr)

    # out <- comparisons(
    #   model = model,
    #   newdata = newdata,
    #   variables = variables,
    #   type = type,
    #   vcov = vcov,
    #   by = by,
    #   conf_level = conf_level,
    #   comparison = comparison,
    #   transform = transform,
    #   cross = cross,
    #   wts = wts,
    #   hypothesis = hypothesis,
    #   equivalence = equivalence,
    #   df = df,
    #   eps = eps,
    #   ...)

    return(out)
}
