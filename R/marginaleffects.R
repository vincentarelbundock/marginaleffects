#' Marginal Effects (Slopes)
#'
#' Partial derivative (slope) of the regression equation with respect to a
#' regressor of interest. The `tidy()` and `summary()` functions can be used to
#' aggregate and summarize the output of `marginaleffects()`. To learn more,
#' read the marginal effects vignette, visit the package website, or scroll
#' down this page for a full list of vignettes:
#' * <https://vincentarelbundock.github.io/marginaleffects/articles/marginaleffects.html>
#' * <https://vincentarelbundock.github.io/marginaleffects/>
#'
#' @section Vignettes and documentation:
#'
#' ```{r child = "vignettes/toc.Rmd"}
#' ```
#' 
#' @details
#' A "marginal effect" is the partial derivative of the regression equation
#' with respect to a variable in the model. This function uses automatic
#' differentiation to compute marginal effects for a vast array of models,
#' including non-linear models with transformations (e.g., polynomials).
#' Uncertainty estimates are computed using the delta method.
#'
#' The `newdata` argument can be used to control the kind of marginal effects to report:
#' 
#' * Average Marginal Effects (AME)
#' * Group-Average Marginal Effects (G-AME)
#' * Marginal Effects at the Mean (MEM) or
#' * Marginal Effects at User-Specified values (aka Marginal Effects at Representative values, MER).
#'
#' See the [marginaleffects vignette for worked-out examples of each kind of marginal effect.](https://vincentarelbundock.github.io/marginaleffects/articles/marginaleffects.html)
#'
#' Numerical derivatives for the `marginaleffects` function are calculated
#' using a simple epsilon difference approach: \eqn{\partial Y / \partial X = (f(X + \varepsilon) - f(X)) / \varepsilon}{dY/dX = (f(X + e) - f(X)) / e},
#' where f is the `predict()` method associated with the model class, and
#' \eqn{\varepsilon}{e} is determined by the `eps` argument.
#'
#' Warning: Some models are particularly sensitive to `eps`, so it is good
#' practice to try different values of this argument.
#'
#' Standard errors for the marginal effects are obtained using the Delta
#' method. See the "Standard Errors" vignette on the package website for
#' details (link above).
#'
#' @param model Model object
#' @param variables `NULL` or character vector. The subset of variables for which to compute marginal effects.
#' * `NULL`: compute contrasts for all the variables in the model object (can be slow). 
#' * Character vector: subset of variables (usually faster).
#' @param newdata `NULL`, data frame, string, or `datagrid()` call. Determines the predictor values for which to compute marginal effects.
#' + `NULL` (default): Unit-level marginal effects for each observed value in the original dataset.
#' + data frame: Unit-level marginal effects for each row of the `newdata` data frame.
#' + string:
#'   - "mean": Marginal Effects at the Mean. Marginal effects when each predictor is held at its mean or mode.
#'   - "median": Marginal Effects at the Median. Marginal effects when each predictor is held at its median or mode.
#'   - "marginalmeans": Marginal Effects at Marginal Means. See Details section below.
#'   - "tukey": Marginal Effects at Tukey's 5 numbers.
#'   - "grid": Marginal Effects on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid()] documentation.
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
#' @param by Character vector of variable names over which to compute group-wise estimates.
#' @param type string indicates the type (scale) of the predictions used to
#' compute marginal effects or contrasts. This can differ based on the model
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
#' contrasts or marginaleffects. These weights only affect the averaging in
#' `tidy()` or `summary()`, and not the unit-level estimates themselves.
#' + string: column name of the weights variable in `newdata`. When supplying a column name to `wts`, it is recommended to supply the original data (including the weights variable) explicitly to `newdata`.
#' + numeric: vector of length equal to the number of rows in the original data or in `newdata` (if supplied). 
#' @param hypothesis specify a hypothesis test or custom contrast using a vector, matrix, string, or string formula.
#' + String:
#'   - "pairwise": pairwise differences between estimates in each row.
#'   - "reference": differences between the estimates in each row and the estimate in the first row.
#'   - "sequential": difference between an estimate and the estimate in the next row.
#'   - "revpairwise", "revreference", "revsequential": inverse of the corresponding hypotheses, as described above.
#' + String formula to specify linear or non-linear hypothesis tests. If the `term` column uniquely identifies rows, terms can be used in the formula. Otherwise, use `b1`, `b2`, etc. to identify the position of each parameter. Examples:
#'   - `hp = drat`
#'   - `hp + drat = 12`
#'   - `b1 + b2 + b3 = 0`
#' + Numeric vector: Weights to compute a linear combination of (custom contrast between) estimates. Length equal to the number of rows generated by the same function call, but without the `hypothesis` argument.
#' + Numeric matrix: Each column is a vector of weights, as describe above, used to compute a distinct linear combination of (contrast between) estimates. The column names of the matrix are used as labels in the output.
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
#'
#' @template model_specific_arguments
#'
#' @return A `data.frame` with one row per observation (per term/group) and several columns:
#' * `rowid`: row number of the `newdata` data frame
#' * `type`: prediction type, as defined by the `type` argument
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `term`: the variable whose marginal effect is computed
#' * `dydx`: marginal effect of the term on the outcome for a given combination of regressor values
#' * `std.error`: standard errors computed by via the delta method.
#' @examplesIf interactive()
#' @examples
#'
#' mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
#' mfx <- marginaleffects(mod)
#' head(mfx)
#'
#' # Average Marginal Effect (AME)
#' summary(mfx)
#' tidy(mfx)
#' plot(mfx)
#'
#' 
#' # Marginal Effect at the Mean (MEM)
#' marginaleffects(mod, newdata = datagrid())
#'
#' # Marginal Effect at User-Specified Values
#' # Variables not explicitly included in `datagrid()` are held at their means
#' marginaleffects(mod,
#'                 newdata = datagrid(hp = c(100, 110)))
#'
#' # Group-Average Marginal Effects (G-AME)
#' # Calculate marginal effects for each observation, and then take the average
#' # marginal effect within each subset of observations with different observed
#' # values for the `cyl` variable:
#' mod2 <- lm(mpg ~ hp * cyl, data = mtcars)
#' mfx2 <- marginaleffects(mod2, variables = "hp", by = "cyl")
#' summary(mfx2)
#'
#' # Marginal Effects at User-Specified Values (counterfactual)
#' # Variables not explicitly included in `datagrid()` are held at their
#' # original values, and the whole dataset is duplicated once for each
#' # combination of the values in `datagrid()`
#' mfx <- marginaleffects(mod,
#'                        newdata = datagrid(hp = c(100, 110),
#'                                           grid_type = "counterfactual"))
#' head(mfx)
#'
#' # Heteroskedasticity robust standard errors
#' marginaleffects(mod, vcov = sandwich::vcovHC(mod))
#'
#' # hypothesis test: is the `hp` marginal effect at the mean equal to the `drat` marginal effect
#' mod <- lm(mpg ~ wt + drat, data = mtcars)
#'
#' marginaleffects(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = "wt = drat")
#' 
#' # same hypothesis test using row indices
#' marginaleffects(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = "b1 - b2 = 0")
#' 
#' # same hypothesis test using numeric vector of weights
#' marginaleffects(
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
#' marginaleffects(
#'     mod,
#'     newdata = "mean",
#'     hypothesis = lc)
#' 
#' @export
marginaleffects <- function(model,
                            newdata = NULL,
                            variables = NULL,
                            vcov = TRUE,
                            conf_level = 0.95,
                            type = NULL,
                            slope = "dydx",
                            by = NULL,
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
                lcall <- c(lcall, list("x" = hush(insight::get_data(model))))
                newdata <- eval.parent(as.call(lcall))
            }
        }

    }

    # marginaleffects() does not support a named list of variables like comparisons()
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

    # report slope, not contrast
    setnames(out, old = "comparison", new = "dydx")

    # clean columns
    stubcols <- c("rowid", "type", "group", "term", "contrast", "hypothesis", "dydx", "std.error", "statistic", "p.value", "conf.low", "conf.high",
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

    # class
    setDF(out)
    class(out) <- setdiff(class(out), "comparisons")
    class(out) <- c("marginaleffects", class(out))
    return(out)
}


#' `meffects()` is a shortcut to `marginaleffects()`
#'
#' @inherit marginaleffects
#' @keywords internal
#' @export
meffects <- marginaleffects

