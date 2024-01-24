#' Slopes (aka Partial derivatives, Marginal Effects, or Trends)
#'
#' @description
#' Partial derivative of the regression equation with respect to a regressor of interest.
#'
#' * `slopes()`: unit-level (conditional) estimates.
#' * `avg_slopes()`: average (marginal) estimates.
#'
#' The `newdata` argument and the `datagrid()` function can be used to control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.
#'
#' See the slopes vignette and package website for worked examples and case studies:
#'
#' * <https://marginaleffects.com/vignettes/slopes.html>
#' * <https://marginaleffects.com/>
#'
#' @details
#' A "slope" or "marginal effect" is the partial derivative of the regression equation
#' with respect to a variable in the model. This function uses automatic
#' differentiation to compute slopes for a vast array of models,
#' including non-linear models with transformations (e.g., polynomials).
#' Uncertainty estimates are computed using the delta method.
#'
#' @param model Model object
#' @param variables Focal variables
#' * `NULL`: compute slopes or comparisons for all the variables in the model object (can be slow).
#' * Character vector: subset of variables (usually faster).
#' @param newdata Grid of predictor values at which we evaluate the slopes.
#' + Warning: Please avoid modifying your dataset between fitting the model and calling a `marginaleffects` function. This can sometimes lead to unexpected results.
#' + `NULL` (default): Unit-level slopes for each observed value in the dataset (empirical distribution). The dataset is retrieved using [insight::get_data()], which tries to extract data from the environment. This may produce unexpected results if the original data frame has been altered since fitting the model.
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid()] documentation.
#' + string:
#'   - "mean": Marginal Effects at the Mean. Slopes when each predictor is held at its mean or mode.
#'   - "median": Marginal Effects at the Median. Slopes when each predictor is held at its median or mode.
#'   - "marginalmeans": Marginal Effects at Marginal Means. See Details section below.
#'   - "tukey": Marginal Effects at Tukey's 5 numbers.
#'   - "grid": Marginal Effects on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
#' @param vcov Type of uncertainty estimates to report (e.g., for robust standard errors). Acceptable values:
#'  * FALSE: Do not compute standard errors. This can speed up computation considerably.
#'  * TRUE: Unit-level standard errors using the default `vcov(model)` variance-covariance matrix.
#'  * String which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Heteroskedasticity and autocorrelation consistent: `"HAC"`
#'    - Mixed-Models degrees of freedom: "satterthwaite", "kenward-roger"
#'    - Other: `"NeweyWest"`, `"KernHAC"`, `"OPG"`. See the `sandwich` package documentation.
#'  * One-sided formula which indicates the name of cluster variables (e.g., `~unit_id`). This formula is passed to the `cluster` argument of the `sandwich::vcovCL` function.
#'  * Square covariance matrix
#'  * Function which returns a covariance matrix (e.g., `stats::vcov(model)`)
#' @param conf_level numeric value between 0 and 1. Confidence level to use to build a confidence interval.
#' @param type string indicates the type (scale) of the predictions used to
#' compute contrasts or slopes. This can differ based on the model
#' type, but will typically be a string such as: "response", "link", "probs",
#' or "zero". When an unsupported string is entered, the model-specific list of
#' acceptable values is returned in an error message. When `type` is `NULL`, the
#' first entry in the error message is used by default.
#' @param slope string indicates the type of slope or (semi-)elasticity to compute:
#' - "dydx": dY/dX
#' - "eyex": dY/dX * Y / X
#' - "eydx": dY/dX * Y
#' - "dyex": dY/dX / X
#' - Y is the predicted value of the outcome; X is the observed value of the predictor.
#' @param wts string or numeric: weights to use when computing average contrasts or slopes. These weights only affect the averaging in `avg_*()` or with the `by` argument, and not the unit-level estimates themselves. Internally, estimates and weights are passed to the `weighted.mean()` function.
#' + string: column name of the weights variable in `newdata`. When supplying a column name to `wts`, it is recommended to supply the original data (including the weights variable) explicitly to `newdata`.
#' + numeric: vector of length equal to the number of rows in the original data or in `newdata` (if supplied).
#' @param hypothesis specify a hypothesis test or custom contrast using a numeric value, vector, or matrix, a string, or a string formula.
#' + Numeric:
#'   - Single value: the null hypothesis used in the computation of Z and p (before applying `transform`).
#'   - Vector: Weights to compute a linear combination of (custom contrast between) estimates. Length equal to the number of rows generated by the same function call, but without the `hypothesis` argument.
#'   - Matrix: Each column is a vector of weights, as describe above, used to compute a distinct linear combination of (contrast between) estimates. The column names of the matrix are used as labels in the output.
#' + String formula to specify linear or non-linear hypothesis tests. If the `term` column uniquely identifies rows, terms can be used in the formula. Otherwise, use `b1`, `b2`, etc. to identify the position of each parameter. The `b*` wildcard can be used to test hypotheses on all estimates. Examples:
#'   - `hp = drat`
#'   - `hp + drat = 12`
#'   - `b1 + b2 + b3 = 0`
#'   - `b* / b1 = 1`
#' + String:
#'   - "pairwise": pairwise differences between estimates in each row.
#'   - "reference": differences between the estimates in each row and the estimate in the first row.
#'   - "sequential": difference between an estimate and the estimate in the next row.
#'   - "revpairwise", "revreference", "revsequential": inverse of the corresponding hypotheses, as described above.
#' + See the Examples section below and the vignette: https://marginaleffects.com/vignettes/hypothesis.html
#' @param p_adjust Adjust p-values for multiple comparisons: "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", or "fdr". See [stats::p.adjust]
#' @param df Degrees of freedom used to compute p values and confidence intervals. A single numeric value between 1 and `Inf`. When `df` is `Inf`, the normal distribution is used. When `df` is finite, the `t` distribution is used. See [insight::get_df] for a convenient function to extract degrees of freedom. Ex: `slopes(model, df = insight::get_df(model))`
#' @param eps NULL or numeric value which determines the step size to use when
#' calculating numerical derivatives: (f(x+eps)-f(x))/eps. When `eps` is
#' `NULL`, the step size is 0.0001 multiplied by the difference between
#' the maximum and minimum values of the variable with respect to which we
#' are taking the derivative. Changing `eps` may be necessary to avoid
#' numerical problems in certain models.
#' @param numderiv string or list of strings indicating the method to use to for the numeric differentiation used in to compute delta method standard errors.
#' + "fdforward": finite difference method with forward differences
#' + "fdcenter": finite difference method with central differences (default)
#' + "richardson": Richardson extrapolation method
#' + Extra arguments can be specified by passing a list to the `numDeriv` argument, with the name of the method first and named arguments following, ex: `numderiv=list("fdcenter", eps = 1e-5)`. When an unknown argument is used, `marginaleffects` prints the list of valid arguments for each method.
#' @param ... Additional arguments are passed to the `predict()` method
#' supplied by the modeling package.These arguments are particularly useful
#' for mixed-effects or bayesian models (see the online vignettes on the
#' `marginaleffects` website). Available arguments can vary from model to
#' model, depending on the range of supported arguments by each modeling
#' package. See the "Model-Specific Arguments" section of the
#' `?slopes` documentation for a non-exhaustive list of available
#' arguments.
#' @inheritParams comparisons
#'
#' @details
#' Numerical derivatives for the `slopes` function are calculated
#' using a simple epsilon difference approach: \eqn{\partial Y / \partial X = (f(X + \varepsilon/2) - f(X-\varepsilon/2)) / \varepsilon}{dY/dX = (f(X + e/2) - f(X-e/2)) / e},
#' where f is the `predict()` method associated with the model class, and
#' \eqn{\varepsilon}{e} is determined by the `eps` argument.
#' @template deltamethod
#' @template model_specific_arguments
#' @template bayesian
#' @template equivalence
#' @template type
#' @template references
#'
#' @return A `data.frame` with one row per observation (per term/group) and several columns:
#' * `rowid`: row number of the `newdata` data frame
#' * `type`: prediction type, as defined by the `type` argument
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `term`: the variable whose marginal effect is computed
#' * `dydx`: slope of the outcome with respect to the term, for a given combination of predictor values
#' * `std.error`: standard errors computed by via the delta method.
#' * `p.value`: p value associated to the `estimate` column. The null is determined by the `hypothesis` argument (0 by default), and p values are computed before applying the `transform` argument. For models of class `feglm`, `Gam`, `glm` and `negbin`, p values are computed on the link scale by default unless the `type` argument is specified explicitly.
#' * `s.value`: Shannon information transforms of p values. How many consecutive "heads" tosses would provide the same amount of evidence (or "surprise") against the null hypothesis that the coin is fair? The purpose of S is to calibrate the analyst's intuition about the strength of evidence encoded in p against a well-known physical phenomenon. See Greenland (2019) and Cole et al. (2020).
#' * `conf.low`: lower bound of the confidence interval (or equal-tailed interval for bayesian models)
#' * `conf.high`: upper bound of the confidence interval (or equal-tailed interval for bayesian models)
#'
#' See `?print.marginaleffects` for printing options.
#'
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
#' @examples
#' # Unit-level (conditional) Marginal Effects
#' mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
#' mfx <- slopes(mod)
#' head(mfx)
#'
#' # Average Marginal Effect (AME)
#' avg_slopes(mod, by = TRUE)
#'
#'
#' # Marginal Effect at the Mean (MEM)
#' slopes(mod, newdata = datagrid())
#'
#' # Marginal Effect at User-Specified Values
#' # Variables not explicitly included in `datagrid()` are held at their means
#' slopes(mod, newdata = datagrid(hp = c(100, 110)))
#'
#' # Group-Average Marginal Effects (G-AME)
#' # Calculate marginal effects for each observation, and then take the average
#' # marginal effect within each subset of observations with different observed
#' # values for the `cyl` variable:
#' mod2 <- lm(mpg ~ hp * cyl, data = mtcars)
#' avg_slopes(mod2, variables = "hp", by = "cyl")
#'
#' # Marginal Effects at User-Specified Values (counterfactual)
#' # Variables not explicitly included in `datagrid()` are held at their
#' # original values, and the whole dataset is duplicated once for each
#' # combination of the values in `datagrid()`
#' mfx <- slopes(mod,
#'               newdata = datagrid(hp = c(100, 110),
#'               grid_type = "counterfactual"))
#' head(mfx)
#'
#' # Heteroskedasticity robust standard errors
#' mfx <- slopes(mod, vcov = sandwich::vcovHC(mod))
#' head(mfx)
#'
#' # hypothesis test: is the `hp` marginal effect at the mean equal to the `drat` marginal effect
#' mod <- lm(mpg ~ wt + drat, data = mtcars)
#'
#' slopes(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = "wt = drat")
#'
#' # same hypothesis test using row indices
#' slopes(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = "b1 - b2 = 0")
#'
#' # same hypothesis test using numeric vector of weights
#' slopes(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = c(1, -1))
#'
#' # two custom contrasts using a matrix of weights
#' lc <- matrix(c(
#'     1, -1,
#'     2, 3),
#'     ncol = 2)
#' colnames(lc) <- c("Contrast A", "Contrast B")
#' slopes(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = lc)
#' 
#' @export
slopes <- function(model,
                   newdata = NULL,
                   variables = NULL,
                   type = NULL,
                   by = FALSE,
                   vcov = TRUE,
                   conf_level = 0.95,
                   slope = "dydx",
                   wts = NULL,
                   hypothesis = NULL,
                   equivalence = NULL,
                   p_adjust = NULL,
                   df = Inf,
                   eps = NULL,
                   numderiv = "fdforward",
                   ...) {

    dots <- list(...)

    # very early, before any use of newdata
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    scall <- rlang::enquo(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, model)

    # build call: match.call() doesn't work well in *apply()
    call_attr <- c(list(
        name = "slopes",
        model = model,
        newdata = newdata,
        variables = variables,
        type = type,
        vcov = vcov,
        by = by,
        conf_level = conf_level,
        slope = slope,
        wts = wts,
        hypothesis = hypothesis,
        df = df,
        eps = eps),
        list(...))
    call_attr <- do.call("call", call_attr)

    # slopes() does not support a named list of variables like comparisons()
    checkmate::assert_character(variables, null.ok = TRUE)

    # slope
    valid <- c("dydx", "eyex", "eydx", "dyex", "dydxavg", "eyexavg", "eydxavg", "dyexavg")
    checkmate::assert_choice(slope, choices = valid)

    # sanity checks and pre-processing
    model <- sanitize_model(model = model, newdata = newdata, wts = wts, vcov = vcov, calling_function = "marginaleffects", ...)
    sanity_dots(model = model, calling_function = "marginaleffects", ...)
    type <- sanitize_type(model = model, type = type, calling_function = "slopes")

    ############### sanity checks are over

    # Bootstrap
    out <- inferences_dispatch(
        INF_FUN = slopes,
        model = model, newdata = newdata, vcov = vcov, variables = variables, type = type,
        conf_level = conf_level,
        by = by,
        wts = wts, slope = slope, hypothesis = hypothesis, ...)
    if (!is.null(out)) {
        return(out)
    }

    out <- comparisons(
        model,
        newdata = newdata,
        variables = variables,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        wts = wts,
        hypothesis = hypothesis,
        equivalence = equivalence,
        df = df,
        p_adjust = p_adjust,
        by = by,
        eps = eps,
        numderiv = numderiv,
        comparison = slope,
        cross = FALSE,
        # secret arguments
        internal_call = TRUE,
        ...)

    data.table::setDT(out)

    attr(out, "vcov.type") <- get_vcov_label(vcov)
    attr(out, "newdata") <- newdata # recall
    attr(out, "call") <- call_attr

    # class
    data.table::setDF(out)
    class(out) <- setdiff(class(out), "comparisons")
    class(out) <- c("slopes", "marginaleffects", class(out))
    return(out)
}




#' Average slopes (aka Average partial derivatives, marginal effects, or trends)
#' @describeIn slopes Average slopes
#' @export
#'
avg_slopes <- function(model,
                       newdata = NULL,
                       variables = NULL,
                       type = NULL,
                       by = TRUE,
                       vcov = TRUE,
                       conf_level = 0.95,
                       slope = "dydx",
                       wts = NULL,
                       hypothesis = NULL,
                       equivalence = NULL,
                       p_adjust = NULL,
                       df = Inf,
                       eps = NULL,
                       numderiv = "fdforward",
                       ...) {

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    # should probably not be nested too deeply in the call stack since we eval.parent() (not sure about this)
    scall <- rlang::enquo(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, model)


    # Bootstrap
    out <- inferences_dispatch(
        INF_FUN = avg_slopes,
        model = model, newdata = newdata, vcov = vcov, variables = variables, type = type,
        conf_level = conf_level, by = by,
        wts = wts, slope = slope, hypothesis = hypothesis, ...)
    if (!is.null(out)) {
        return(out)
    }


    out <- slopes(
        model = model,
        newdata = newdata,
        variables = variables,
        type = type,
        vcov = vcov,
        conf_level = conf_level,
        by = by,
        slope = slope,
        wts = wts,
        hypothesis = hypothesis,
        equivalence = equivalence,
        p_adjust = p_adjust,
        df = df,
        eps = eps,
        numderiv = numderiv,
        ...)

    return(out)
}
