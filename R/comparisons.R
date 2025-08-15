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
    # init
    if (inherits(model, "marginaleffects_internal")) {
        mfx <- model
    } else {
        call <- construct_call(model, "comparisons")
        model <- sanitize_model(model, call = call, newdata = newdata, wts = wts, vcov = vcov, by = by, ...)
        mfx <- new_marginaleffects_internal(
            call = call,
            model = model,
            by = by,
            comparison = comparison,
            cross = cross,
            eps = eps
        )
    }

    scall <- rlang::enquo(newdata)
    mfx <- add_newdata(mfx, 
        scall, 
        newdata = newdata, 
        by = by, 
        wts = wts, 
        cross = cross, 
        comparison = comparison)

    # sanity checks
    dots <- list(...)
    sanity_dots(mfx@model, ...)

    # multiple imputation
    if (inherits(mfx@model, c("mira", "amest"))) {
        out <- process_imputation(mfx)
        return(out)
    }

    # inferences() dispatch
    methods <- c("rsample", "boot", "fwb", "simulation")
    if (isTRUE(checkmate::check_choice(vcov, methods))) {
        inferences_method <- vcov
        vcov <- FALSE
    } else {
        inferences_method <- NULL
    }

    # very early, before any use of newdata
    newdata <- mfx@newdata

    # misc
    mfx@conf_level <- sanitize_conf_level(conf_level, ...)
    mfx <- add_numderiv(mfx, numderiv)
    checkmate::assert_number(eps, lower = 1e-10, null.ok = TRUE)

    # misc sanitation
    mfx <- add_by(mfx, by)
    sanity_reserved(mfx@model, mfx@modeldata)

    cross <- sanitize_cross(cross, variables, mfx@model)
    mfx@type <- sanitize_type(
        model = mfx@model,
        type = type,
        calling_function = mfx@calling_function
    )
    sanity_comparison(comparison)

    # transforms
    transform <- sanitize_transform(transform)

    # after sanitize_newdata
    mfx <- add_variables(
        variables = variables,
        mfx = mfx
    )

    # get dof before transforming the vcov arg
    # add_degrees_of_freedom() produces a weird warning on non lmerMod. We can skip them
    # because get_vcov() will produce an informative error later.
    mfx <- add_degrees_of_freedom(mfx = mfx, df = df, by = by, hypothesis = hypothesis, vcov = vcov)

    mfx@vcov_type <- get_vcov_label(vcov)
    mfx@vcov_model <- get_vcov(mfx@model, vcov = vcov, type = mfx@type, ...)

    predictors <- mfx@variables$conditional

    mfx <- add_hypothesis(mfx, hypothesis)

    ############### sanity checks are over

    # compute contrasts and standard errors
    args <- list(
        mfx = mfx,
        variables = predictors,
        cross = cross
    )
    dots[["modeldata"]] <- NULL # dont' pass twice
    args <- utils::modifyList(args, dots)
    contrast_data <- do.call("get_comparisons_data", args)

    args <- list(
        mfx = mfx,
        variables = predictors,
        type = mfx@type,
        original = contrast_data[["original"]],
        hi = contrast_data[["hi"]],
        lo = contrast_data[["lo"]],
        by = by,
        cross = cross,
        hypothesis = mfx@hypothesis
    )
    args <- utils::modifyList(args, dots)
    cmp <- do.call("get_comparisons", args)

    hyp_by <- attr(cmp, "hypothesis_function_by")

    # bayesian posterior
    mfx@draws <- attr(cmp, "posterior_draws")

    # standard errors via delta method
    if (is.null(mfx@draws) && 
        !isFALSE(vcov) && 
        isTRUE(checkmate::check_matrix(mfx@vcov_model))) {

        idx <- intersect(colnames(cmp), c("group", "term", "contrast"))
        idx <- cmp[, (idx), drop = FALSE]
        fun <- function(...) {
            get_comparisons(..., verbose = FALSE)$estimate
        }
        args <- list(
            mfx = mfx,
            model_perturbed = mfx@model,
            vcov = mfx@vcov_model,
            type = mfx@type,
            FUN = fun,
            index = idx,
            variables = predictors,
            hypothesis = mfx@hypothesis,
            hi = contrast_data$hi,
            lo = contrast_data$lo,
            original = contrast_data$original,
            numderiv = numderiv
        )
        args <- utils::modifyList(args, dots)
        se <- do.call("get_se_delta", args)
        mfx@jacobian <- attr(se, "jacobian")
        cmp$std.error <- as.vector(as.numeric(se)) # drop attributes
        mfx@draws <- NULL
    }

    # merge original data back in
    if ((is.null(by) || isFALSE(by)) && "rowid" %in% colnames(cmp)) {
        if ("rowid" %in% colnames(mfx@newdata)) {
            idx <- c(
                "rowid",
                "rowidcf",
                "term",
                "contrast",
                "by",
                setdiff(colnames(contrast_data$original), colnames(cmp))
            )
            idx <- intersect(idx, colnames(contrast_data$original))
            tmp <- contrast_data$original[, ..idx, drop = FALSE]
            # contrast_data is duplicated to compute contrasts for different terms or pairs
            bycols <- intersect(colnames(tmp), colnames(cmp))
            idx <- duplicated(tmp, by = bycols)
            tmp <- tmp[!idx]
            cmp <- merge(cmp, tmp, all.x = TRUE, by = bycols, sort = FALSE)
            # HACK: relies on NO sorting at ANY point
        } else {
            idx <- setdiff(colnames(contrast_data$original), colnames(cmp))
            cmp <- data.table(cmp, contrast_data$original[, ..idx])
        }
    }

    # meta info
    cmp <- get_ci(cmp, mfx)

    # clean rows and columns
    # WARNING: we cannot sort rows at the end because `get_hypothesis()` is
    # applied in the middle, and it must already be sorted in the final order,
    # otherwise, users cannot know for sure what is going to be the first and
    # second rows, etc.
    cmp <- sort_columns(cmp, mfx@newdata, by)

    # equivalence tests
    cmp <- equivalence(cmp, equivalence = equivalence, df = mfx@df, ...)

    # after draws attribute
    cmp <- backtransform(cmp, transform, draws = mfx@draws)

    # remove weights column (now handled by add_attributes)
    cmp[["marginaleffects_wts_internal"]] <- NULL

    out <- cmp

    data.table::setDF(out)

    out <- add_attributes(out, mfx,
        hypothesis_by = hyp_by)

    if (inherits(mfx@model, "brmsfit")) {
        insight::check_if_installed("brms")
        mfx@draws_chains <- brms::nchains(mfx@model) 
    }

    class(out) <- c("comparisons", class(out))

    if (!is.null(inferences_method)) {
        out <- inferences(out, method = inferences_method)
    }

    attr(out, "marginaleffects") <- mfx

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
    call_attr <- construct_call(model, "comparisons")

    out <- eval.parent(call_attr)

    return(out)
}
