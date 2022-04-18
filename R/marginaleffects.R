#' Marginal Effects (Slopes)
#'
#' This function calculates marginal effects (slopes) for each row of the
#' dataset. The resulting object can processed by the `tidy()` or `summary()`
#' functions, which compute Average Marginal Effects (AME). The `datagrid()`
#' function and the `newdata` argument can be used to calculate Marginal
#' Effects at the Mean (MEM) or Marginal Effects at User-Specified values (aka
#' Marginal Effects at Representative values, MER). Additional information can
#' be found in the Details and Examples sections below, and in the vignette on
#' the `marginaleffects` website.
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
#' @param model Model object
#' @param variables Variables to consider (character vector). `NULL`
#'   calculates marginal effects for all terms in the model object.
#' @param newdata A dataset over which to compute marginal effects. `NULL` uses
#'   the original data used to fit the model.
#' @param vcov Matrix or boolean
#'   + FALSE: does not compute unit-level standard errors. This can speed up computation considerably.
#'   + TRUE: computes unit-level standard errors using the default `vcov(model)` variance-covariance matrix.
#'   + Named square matrix: computes standard errors with a user-supplied variance-covariance matrix. This matrix must be square and have dimensions equal to the number of coefficients in `get_coef(model)`.
#' @param vcov Variance-covariance matrix used to compute uncertainty estimates (e.g., for robust standard errors). Acceptable values are:
#'  * TRUE: Default uncertainty estimates of the model object.
#'  * FALSE: Omit uncertainty estimates.
#'  * A string which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Heteroskedasticity and autocorrelation consistent: `"HAC"`
#'    - Other: `"NeweyWest"`, `"KernHAC"`, `"OPG"`. See the `sandwich` package documentation.
#'  * A one-sided formula which indicates the name of cluster variables (e.g., `~unit_id`). This formula is passed to the `cluster` argument of the `sandwich::vcovCL` function.
#'  * A square covariance matrix
#'  * A function which returns a covariance matrix (e.g., `stats::vcov(model)`)
#' @param conf.level The confidence level to use for the confidence interval if
#'   `conf.int=TRUE`. Must be strictly greater than 0 and less than 1. Defaults
#'   to 0.95, which corresponds to a 95 percent confidence interval.
#' @param type Type(s) of prediction as string or character vector. This can
#'   differ based on the model type, but will typically be a string such as:
#'   "response", "link", "probs", or "zero".
#' @param ... Additional arguments are passed to the `predict()` method used to
#'   compute adjusted predictions. These arguments are particularly useful for
#'   mixed-effects or bayesian models (see the online vignettes on the
#'   `marginaleffects` website). Available arguments can vary from model to
#'   model, depending on the range of supported arguments by each modeling
#'   package. See the "Model-Specific Arguments" section of the
#'   `?marginaleffects` documentation for a non-exhaustive list of available
#'   arguments.
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
#'                                           grid.type = "counterfactual"))
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
                            conf.level = 0.95,
                            type = "response",
                            ...) {


    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
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
    model <- sanity_model(model = model, calling_function = "marginaleffects", ...)
    sanity_dots(model = model, ...)
    sanity_type(model = model, type = type, calling_function = "marginaleffects")
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
        conf.level = conf.level,
        type = type,
        contrast_numeric = "dydx",
        contrast_factor = "reference",
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
    stubcols <- c("rowid", "type", "group", "term", "contrast", "dydx", "std.error", "conf.low", "conf.high",
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
        out[contrast == "dydx", "contrast" := ""]
        if (all(out$contrast == "")) {
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

    return(out)
}


#' `meffects()` is a shortcut to `marginaleffects()`
#'
#' @inherit marginaleffects
#' @keywords internal
#' @export
meffects <- marginaleffects
