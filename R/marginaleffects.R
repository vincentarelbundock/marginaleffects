#' Marginal Effects (Slopes)
#'
#' This function calculates marginal effects (slopes) for each row of the
#' dataset. The resulting object can processed by the `tidy()` or `summary()`
#' functions, which compute Average Marginal Effects (AME) or Group-Average
#' Marginal Effects (G-AME). The `datagrid()` function and the `newdata`
#' argument can be used to calculate Marginal Effects at the Mean (MEM) or
#' Marginal Effects at User-Specified values (aka Marginal Effects at
#' Representative values, MER). For more information, see the Details and
#' Examples sections below, and in the vignettes on the `marginaleffects`
#' website: <https://vincentarelbundock.github.io/marginaleffects/>
#' * [Getting Started](https://vincentarelbundock.github.io/marginaleffects/#getting-started)
#' * [Marginal Effects Vignette](https://vincentarelbundock.github.io/marginaleffects/articles/mfx02_mfx.html)
#' * [Supported Models](https://vincentarelbundock.github.io/marginaleffects/articles/mfx06_supported_models.html)
#' * Case Studies
#'    - [Bayesian analyses with `brms`](https://vincentarelbundock.github.io/marginaleffects/articles/brms.html)
#'    - [Mixed effects models](https://vincentarelbundock.github.io/marginaleffects/articles/lme4.html)
#'    - [Generalized Additive Models](https://vincentarelbundock.github.io/marginaleffects/articles/gam.html)
#'    - [Multinomial Logit and Discrete Choice Models](https://vincentarelbundock.github.io/marginaleffects/articles/mlogit.html)
#'    - [Tables and plots](https://vincentarelbundock.github.io/marginaleffects/articles/modelsummary.html)
#'    - [Robust standard errors and more](https://vincentarelbundock.github.io/marginaleffects/articles/sandwich.html)
#'    - [Transformations and Custom Contrasts: Adjusted Risk Ratio Example](https://vincentarelbundock.github.io/marginaleffects/articles/transformation.html)
#'
#' A "marginal effect" is the partial derivative of the regression equation
#' with respect to a variable in the model. This function uses automatic
#' differentiation to compute marginal effects for a vast array of models,
#' including non-linear models with transformations (e.g., polynomials).
#' Uncertainty estimates are computed using the delta method.
#'
#' A detailed vignette on marginal effects and a list of supported models can
#' be found on the package website:
#'
#' https://vincentarelbundock.github.io/marginaleffects/
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
#' method. See the "Technical Notes" vignette on the package website for details.
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
#' @param eps A numeric value specifying the “step” size to use when
#' calculating numerical derivatives. See the Details section below. Warning:
#' the marginal effects computed for certain models can be sensitive to the
#' choice of step (e.g., Bayesian mixed effects).
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
#' @export
marginaleffects <- function(model,
                            newdata = NULL,
                            variables = NULL,
                            vcov = TRUE,
                            conf_level = 0.95,
                            type = "response",
                            eps = 1e-4,
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
    }

    # modelbased::visualisation_matrix attaches useful info for plotting
    attributes_newdata <- attributes(newdata)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_newdata) %in% idx
    attributes_newdata <- attributes_newdata[idx]

    # sanity checks and pre-processing
    model <- sanitize_model(model = model, newdata = newdata, calling_function = "marginaleffects", ...)
    sanity_dots(model = model, calling_function = "marginaleffects", ...)
    sanity_type(model = model, type = type, calling_function = "marginaleffects")
    conf_level <- sanitize_conf_level(conf_level, ...)
    newdata <- sanity_newdata(model, newdata)
    variables <- sanitize_variables(model, newdata, variables)

    # variables is a list but we need a vector (and we drop cluster)
    variables_vec <- unique(unlist(variables))
    # this won't be triggered for multivariate outcomes in `brms`, which
    # produces a list of lists where top level names correspond to names of the
    # outcomes. There should be a more robust way to handle those, but it seems
    # to work for now.
    if ("conditional" %in% names(variables)) {
        # unlist() needed for sampleSelection objects, which nest "outcome" and "selection" variables
        variables_vec <- intersect(variables_vec, unlist(variables[["conditional"]]))
    }

    out <- comparisons(
        model,
        newdata = newdata,
        variables = variables_vec,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
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
    stubcols <- c("rowid", "type", "group", "term", "contrast", "dydx", "std.error", "statistic", "p.value", "conf.low", "conf.high",
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
