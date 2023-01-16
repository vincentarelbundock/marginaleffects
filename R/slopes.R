#' Slopes (aka "Partial derivatives", "Marginal Effects", or "Trends")
#'
#' @description
#' Partial derivative of the regression equation with respect to a regressor of
#' interest. Slopes are typically "conditional" (or "unit-level") estimates: they will
#' typically vary based on the values of predictors in the model. By default,
#' the `slopes()` function thus returns one estimate of the slope for each row of
#' the dataset used to fit a model. 
#' 
#' The `newdata` argument controls where slopes are evaluated in the predictor
#' space: "at observed values", "at the mean", "at representative values", etc. 
#' 
#' The `averages()` function or `by` argument can aggregate
#' unit-level estimates into an "average slope".
#' 
#' See the slopes vignette and package website for worked examples and case studies:
#' 
#' * <https://vincentarelbundock.github.io/marginaleffects/articles/slopes.html>
#' * <https://vincentarelbundock.github.io/marginaleffects/>
#' 
#' @details
#' A "slope" or "marginal effect" is the partial derivative of the regression equation
#' with respect to a variable in the model. This function uses automatic
#' differentiation to compute slopes for a vast array of models,
#' including non-linear models with transformations (e.g., polynomials).
#' Uncertainty estimates are computed using the delta method.
#'
#' Numerical derivatives for the `slopes` function are calculated
#' using a simple epsilon difference approach: \eqn{\partial Y / \partial X = (f(X + \varepsilon) - f(X)) / \varepsilon}{dY/dX = (f(X + e) - f(X)) / e},
#' where f is the `predict()` method associated with the model class, and
#' \eqn{\varepsilon}{e} is determined by the `eps` argument.
#'
#' Warning: Some models are particularly sensitive to `eps`, so it is good
#' practice to try different values of this argument.
#'
#' Standard errors for the slopes are obtained using the Delta
#' method. See the "Standard Errors" vignette on the package website for
#' details (link above).
#'
#' @section Vignettes and documentation:
#'
#' ```{r child = "vignettes/toc.Rmd"}
#' ```
#' 
#' @param model Model object
#' @param variables `NULL` or character vector. The subset of variables for which to compute slopes.
#' * `NULL`: compute contrasts for all the variables in the model object (can be slow). 
#' * Character vector: subset of variables (usually faster).
#' @param newdata `NULL`, data frame, string, or `datagrid()` call. Determines the predictor values for which to compute slopes.
#' + `NULL` (default): Unit-level slopes each observed value in the original dataset.
#' + data frame: Unit-level slopes for each row of the `newdata` data frame.
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
#' default value is used. This default is the first model-related row in
#' the `marginaleffects:::type_dictionary` dataframe.
#' @param slope string indicates the type of slope or (semi-)elasticity to compute:
#' - "dydx": dY/dX
#' - "eyex": dY/dX * Y / X
#' - "eydx": dY/dX * Y
#' - "dyex": dY/dX / X
#' @param wts string or numeric: weights to use when computing average
#' contrasts or slopes. These weights only affect the averaging in
#' `averages()`, and not the unit-level estimates themselves.
#' + string: column name of the weights variable in `newdata`. When supplying a column name to `wts`, it is recommended to supply the original data (including the weights variable) explicitly to `newdata`.
#' + numeric: vector of length equal to the number of rows in the original data or in `newdata` (if supplied). 
#' @param hypothesis specify a hypothesis test or custom contrast using a numeric value, vector, or matrix, a string, or a string formula.
#' + Numeric:
#'   - Single value: the null hypothesis used in the computation of Z and p (before applying `transform_post`). 
#'   - Vector: Weights to compute a linear combination of (custom contrast between) estimates. Length equal to the number of rows generated by the same function call, but without the `hypothesis` argument.
#'   - Matrix: Each column is a vector of weights, as describe above, used to compute a distinct linear combination of (contrast between) estimates. The column names of the matrix are used as labels in the output.
#' + String formula to specify linear or non-linear hypothesis tests. If the `term` column uniquely identifies rows, terms can be used in the formula. Otherwise, use `b1`, `b2`, etc. to identify the position of each parameter. Examples:
#'   - `hp = drat`
#'   - `hp + drat = 12`
#'   - `b1 + b2 + b3 = 0`
#' + String:
#'   - "pairwise": pairwise differences between estimates in each row.
#'   - "reference": differences between the estimates in each row and the estimate in the first row.
#'   - "sequential": difference between an estimate and the estimate in the next row.
#'   - "revpairwise", "revreference", "revsequential": inverse of the corresponding hypotheses, as described above.
#' + See the Examples section below and the vignette: https://vincentarelbundock.github.io/marginaleffects/articles/hypothesis.html
#' @param eps NULL or numeric value which determines the step size to use when
#' calculating numerical derivatives: (f(x+eps)-f(x))/eps. When `eps` is
#' `NULL`, the step size is 0.0001 multiplied by the difference between
#' the maximum and minimum values of the variable with respect to which we
#' are taking the derivative. Changing `eps` may be necessary to avoid
#' numerical problems in certain models.
#' @param ... Additional arguments are passed to the `predict()` method
#' supplied by the modeling package.These arguments are particularly useful
#' for mixed-effects or bayesian models (see the online vignettes on the
#' `marginaleffects` website). Available arguments can vary from model to
#' model, depending on the range of supported arguments by each modeling
#' package. See the "Model-Specific Arguments" section of the
#' `?marginaleffects` documentation for a non-exhaustive list of available
#' arguments.
#' @inheritParams comparisons
#'
#' @template model_specific_arguments
#' @template bayesian
#'
#' @return A `data.frame` with one row per observation (per term/group) and several columns:
#' * `rowid`: row number of the `newdata` data frame
#' * `type`: prediction type, as defined by the `type` argument
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `term`: the variable whose marginal effect is computed
#' * `dydx`: slope of the outcome with respect to the term, for a given combination of predictor values
#' * `std.error`: standard errors computed by via the delta method.
#' @examplesIf interactive()
#' @examples
#'
#' # Unit-level (conditional) Marginal Effects
#' mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
#' mfx <- slopes(mod)
#' head(mfx)
#'
#' # Average Marginal Effect (AME)
#' slopes(mod, by = TRUE)
#' averages(mfx)
#' tidy(mfx)
#' plot(mfx)
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
#' mfx2 <- slopes(mod2, variables = "hp", by = "cyl")
#' averages(mfx2)
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
                   vcov = TRUE,
                   by = FALSE,
                   conf_level = 0.95,
                   slope = "dydx",
                   wts = NULL,
                   hypothesis = NULL,
                   eps = NULL,
                   ...) {


    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    # should probably not be nested too deeply in the call stack since we eval.parent() (not sure about this)
    scall <- substitute(newdata)
    if (is.call(scall)) {
        lcall <- as.list(scall)
        fun_name <- as.character(scall)[1]
        if (fun_name %in% c("datagrid", "datagridcf", "typical", "counterfactual")) {
            if (!any(c("model", "newdata") %in% names(lcall))) {
                lcall <- c(lcall, list("model" = model))
                newdata <- eval.parent(as.call(lcall))
            }
        } else if (fun_name == "visualisation_matrix") {
            if (!"x" %in% names(lcall)) {
                lcall <- c(lcall, list("x" = get_modeldata(model)))
                newdata <- eval.parent(as.call(lcall))
            }
        }

    }

    # slopes() does not support a named list of variables like comparisons()
    checkmate::assert_character(variables, null.ok = TRUE)

    # slope
    valid <- c("dydx", "eyex", "eydx", "dyex", "dydxavg", "eyexavg", "eydxavg", "dyexavg")
    checkmate::assert_choice(slope, choices = valid)

    # sanity checks and pre-processing
    model <- sanitize_model(model = model, newdata = newdata, wts = wts, vcov = vcov, calling_function = "marginaleffects", ...)
    sanity_dots(model = model, calling_function = "marginaleffects", ...)
    type <- sanitize_type(model = model, type = type, calling_function = "marginaleffects")

    out <- comparisons(
        model,
        newdata = newdata,
        variables = variables,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        wts = wts,
        hypothesis = hypothesis,
        by = by,
        eps = eps,
        contrast_factor = "reference",
        contrast_numeric = 1,
        # hard-coded. Users should use comparisons() for more flexibility
        transform_pre = slope,
        cross = FALSE,
        # secret arguments
        internal_call = TRUE,
        ...)

    setDT(out)

    # clean columns
    stubcols <- c("rowid", "type", "group", "term", "contrast", "hypothesis", "dydx", "estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, ..cols]

    if ("group" %in% colnames(out) && all(out$group == "main_marginaleffect")) {
        out$group <- NULL
    }

    # return contrast column only when relevant
    if ("contrast" %in% colnames(out)) {
        out[is.na(contrast), "contrast" := ""]
        out[contrast == "dydx", "contrast" := "dY/dX"]
        if (all(out$contrast == "dY/dX")) {
            out[, "contrast" := NULL]
        }
    }

    attr(out, "vcov.type") <- get_vcov_label(vcov)
    attr(out, "call") <- match.call()
    # save newdata=datagrid() for use in recall()
    attr(out, "newdata") <- newdata

    # class
    setDF(out)
    class(out) <- setdiff(class(out), "comparisons")
    class(out) <- c("slopes", "marginaleffects", class(out))
    return(out)
}


#' `meffects()` is an alias to `slopes()`
#'
#' This alias is kept for backward compatibility and because some users may prefer that name.
#' @inherit marginaleffects
#' @keywords internal
#' @export
meffects <- slopes


#' `marginaleffects()` is an alias to `slopes()`
#' 
#' This alias is kept for backward compatibility and because some users may prefer that name.
#'
#' @inherit marginaleffects
#' @keywords internal
#' @export
marginaleffects <- slopes
