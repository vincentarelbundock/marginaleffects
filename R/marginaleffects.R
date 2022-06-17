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
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid()] documentation.
#' @param vcov Type of uncertainty estimates to report (e.g., for robust standard errors). Acceptable values:
#'  * FALSE: Do not compute standard errors. This can speed up computation considerably.
#'  * TRUE: Unit-level standard errors using the default `vcov(model)` variance-covariance matrix.
#'  * String which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Heteroskedasticity and autocorrelation consistent: `"HAC"`
#'    - Other: `"NeweyWest"`, `"KernHAC"`, `"OPG"`. See the `sandwich` package documentation.
#'  * One-sided formula which indicates the name of cluster variables (e.g., `~unit_id`). This formula is passed to the `cluster` argument of the `sandwich::vcovCL` function.
#'  * Square covariance matrix
#'  * Function which returns a covariance matrix (e.g., `stats::vcov(model)`)
#' @param conf_level numeric value between 0 and 1. Confidence level to use to build a confidence interval.
#' @param type string indicates the type (scale) of the predictions used to
#' compute marginal effects or contrasts. This can differ based on the model
#' type, but will typically be a string such as: "response", "link", "probs",
#' or "zero". When an unsupported string is entered, the model-specific list of
#' acceptable values is returned in an error message.
#' @param wts string or numeric: weights to use when computing average
#' contrasts or marginaleffects. These weights only affect the averaging in
#' `tidy()` or `summary()`, and not the unit-level estimates themselves.
#' + string: column name of the weights variable in `newdata`. When supplying a column name to `wts`, it is recommended to supply the original data (including the weights variable) explicitly to `newdata`.
#' + numeric: vector of length equal to the number of rows in the original data or in `newdata` (if supplied). 
#' @param hypothesis specify a hypothesis test or custom contrast using a vector, matrix, string, or string formula.
#' + String:
#'   - "pairwise": pairwise differences between estimates in each row.
#'   - "reference": differences between the estimates in each row and the estimate in the first row.
#' + String formula to specify linear or non-linear hypothesis tests. If the `term` column uniquely identifies rows, terms can be used in the formula. Otherwise, use `b1`, `b2`, etc. to identify the position of each parameter. Examples:
#'   - `hp = drat`
#'   - `hp + drat = 12`
#'   - `b1 + b2 + b3 = 0`
#' + Numeric vector: Weights to compute a linear combination of (custom contrast between) estimates. Length equal to the number of rows generated by the same function call, but without the `hypothesis` argument.
#' + Numeric matrix: Each column is a vector of weights, as describe above, used to compute a distinct linear combination of (contrast between) estimates.
#' + See the Examples section below and the vignette: https://vincentarelbundock.github.io/marginaleffects/articles/hypothesis.html
#' @param eps NULL or numeric value which determines the step size to use when
#' calculating numerical derivatives: (f(x+eps)-f(x))/eps. When `eps` is
#' `NULL`, the step size is step to 0.0001 multiplied by the range of the
#' variable with respect to which we are taking the derivative. Changing this
#' value may be necessary to avoid numerical problems in certain models.
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
#' mfx2 <- marginaleffects(mod2, variables = "hp")
#' summary(mfx2, by = "cyl")
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
                            type = "response",
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
        if (fun_name %in% c("datagrid", "typical", "counterfactual")) {
            if (!any(c("model", "newdata") %in% names(lcall))) {
                lcall <- c(lcall, list("model" = model))
                newdata <- eval.parent(as.call(lcall))
            }
        } else if (fun_name == "visualisation_matrix") {
            if (!"x" %in% names(lcall)) {
                lcall <- c(lcall, list("x" = insight::get_data(model)))
                newdata <- eval.parent(as.call(lcall))
            }
        }

    } else {
        if (is.null(newdata) && !is.null(hypothesis)) {
            newdata <- "mean"
            msg <- format_msg(
            'The `hypothesis` argument of the `marginaleffects()` function must be used in
            conjunction with the `newdata` argument. `newdata` was switched from NULL to
            "mean" automatically.')
            warning(msg, call. = FALSE)
        }
    }

    # modelbased::visualisation_matrix attaches useful info for plotting
    attributes_newdata <- attributes(newdata)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_newdata) %in% idx
    attributes_newdata <- attributes_newdata[idx]

    # sanity checks and pre-processing
    model <- sanitize_model(model = model, newdata = newdata, wts = wts, calling_function = "marginaleffects", ...)
    sanity_dots(model = model, calling_function = "marginaleffects", ...)
    sanitize_type(model = model, type = type, calling_function = "marginaleffects")
    conf_level <- sanitize_conf_level(conf_level, ...)
    newdata <- sanity_newdata(model, newdata)
    variables_list <- sanitize_variables(model, newdata, variables)
    eps <- sanitize_eps(eps = eps, model = model, variables = variables_list)

    # matrix columns not supported
    matrix_columns <- attr(newdata, "matrix_columns")
    if (any(matrix_columns %in% c(names(variables), variables))) {
        msg <- "Matrix columns are not supported by the `variables` argument."
        stop(msg, call. = FALSE)
    }

    # weights
    sanity_wts(wts, newdata) # after sanity_newdata
    if (!is.null(wts) && !isTRUE(checkmate::check_string(wts))) {
        newdata[["marginaleffects_wts"]] <- wts
        wts <- "marginaleffects_wts"
    }

    # variables is a list but we need a vector (and we drop cluster)
    variables_vec <- unique(unlist(variables_list))
    # this won't be triggered for multivariate outcomes in `brms`, which
    # produces a list of lists where top level names correspond to names of the
    # outcomes. There should be a more robust way to handle those, but it seems
    # to work for now.
    if ("conditional" %in% names(variables_list)) {
        # unlist() needed for sampleSelection objects, which nest "outcome" and "selection" variables
        variables_vec <- intersect(variables_vec, unlist(variables_list[["conditional"]]))
    }

    variables_vec <- setdiff(variables_vec, matrix_columns)

    out <- comparisons(
        model,
        newdata = newdata,
        variables = variables_vec,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        wts = wts,
        hypothesis = hypothesis,
        eps = eps,
        # hard-coded. Users should use comparisons() for more flexibility
        transform_pre = "difference",
        contrast_numeric = "dydx",
        contrast_factor = "reference",
        interaction = FALSE,
        # secret arguments
        internal_call = TRUE,
        ...)

    setDT(out)

    # report slope, not contrast
    setnames(out, old = "comparison", new = "dydx")

    # comparisons() useful info
    attributes_comparisons <- attributes(out)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_comparisons) %in% idx
    attributes_comparisons <- attributes_comparisons[idx]

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

    # class
    setDF(out)
    class(out) <- setdiff(class(out), "comparisons")
    class(out) <- c("marginaleffects", class(out))

    # restore attributes
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }
    for (a in names(attributes_comparisons)) {
        if (!a %in% names(attributes(out))) {
            attr(out, a) <- attributes_comparisons[[a]]
        }
    }

    attr(out, "vcov.type") <- get_vcov_label(vcov)

    return(out)
}


#' `meffects()` is a shortcut to `marginaleffects()`
#'
#' @inherit marginaleffects
#' @keywords internal
#' @export
meffects <- marginaleffects

