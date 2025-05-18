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
#' * [https://marginaleffects.com/chapters/slopes.html](https://marginaleffects.com/chapters/slopes.html)
#' * [https://marginaleffects.com/](https://marginaleffects.com/)
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
#' + [subset()] call with a single argument to select a subset of the dataset used to fit the model, ex: `newdata = subset(treatment == 1)`
#' + [dplyr::filter()] call with a single argument to select a subset of the dataset used to fit the model, ex: `newdata = filter(treatment == 1)`
#' + string:
#'   - "mean": Slopes evaluated when each predictor is held at its mean or mode.
#'   - "median": Slopes evaluated when each predictor is held at its median or mode.
#'   - "balanced": Slopes evaluated on a balanced grid with every combination of categories and numeric variables held at their means.
#'   - "tukey": Slopes evaluated at Tukey's 5 numbers.
#'   - "grid": Slopes evaluated on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
#' @param vcov Type of uncertainty estimates to report (e.g., for robust standard errors). Acceptable values:
#'  * FALSE: Do not compute standard errors. This can speed up computation considerably.
#'  * TRUE: Unit-level standard errors using the default `vcov(model)` variance-covariance matrix.
#'  * String which indicates the kind of uncertainty estimates to return.
#'    - Heteroskedasticity-consistent: `"HC"`, `"HC0"`, `"HC1"`, `"HC2"`, `"HC3"`, `"HC4"`, `"HC4m"`, `"HC5"`. See `?sandwich::vcovHC`
#'    - Heteroskedasticity and autocorrelation consistent: `"HAC"`
#'    - Mixed-Models degrees of freedom: "satterthwaite", "kenward-roger"
#'    - Other: `"NeweyWest"`, `"KernHAC"`, `"OPG"`. See the `sandwich` package documentation.
#'    - "rsample", "boot", "fwb", and "simulation" are passed to the `method` argument of the `inferences()` function. To customize the bootstrap or simulation process, call `inferences()` directly.
#'
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
#' @param wts logical, string or numeric: weights to use when computing average predictions, contrasts or slopes. These weights only affect the averaging in `avg_*()` or with the `by` argument, and not unit-level estimates. See `?weighted.mean`
#' + string: column name of the weights variable in `newdata`. When supplying a column name to `wts`, it is recommended to supply the original data (including the weights variable) explicitly to `newdata`.
#' + numeric: vector of length equal to the number of rows in the original data or in `newdata` (if supplied).
#' + FALSE: Equal weights.
#' + TRUE: Extract weights from the fitted object with `insight::find_weights()` and use them when taking weighted averages of estimates. Warning: `newdata=datagrid()` returns a single average weight, which is equivalent to using `wts=FALSE`
#' @param hypothesis specify a hypothesis test or custom contrast using a number , formula, string equation, vector, matrix, or function.
#' + Number: The null hypothesis used in the computation of Z and p (before applying `transform`).
#' + String: Equation to specify linear or non-linear hypothesis tests. Two-tailed tests must include an equal `=` sign. One-tailed tests must start with `<` or `>`. If the terms in `coef(object)` uniquely identify estimates, they can be used in the formula. Otherwise, use `b1`, `b2`, etc. to identify the position of each parameter. The `b*` wildcard can be used to test hypotheses on all estimates. When the hypothesis string represents a two-sided equation, the `estimate` column holds the value of the left side minus the right side of the equation. If a named vector is used, the names are used as labels in the output. Examples:
#'   - `hp = drat`
#'   - `hp + drat = 12`
#'   - `b1 + b2 + b3 = 0`
#'   - `b* / b1 = 1`
#'   - `<= 0`
#'   - `>= -3.5`
#'   - `b1 >= 10`
#' + Formula: `lhs ~ rhs | group`
#'   + `lhs`
#'     - `ratio` (null = 1)
#'     - `difference` (null = 0)
#'     - Leave empty for default value
#'   + `rhs`
#'     - `pairwise` and `revpairwise`: pairwise differences between estimates in each row.
#'     - `reference`: differences between the estimates in each row and the estimate in the first row.
#'     - `sequential`: difference between an estimate and the estimate in the next row.
#'     - `meandev`: difference between an estimate and the mean of all estimates.
#'     - `meanotherdev: difference between an estimate and the mean of all other estimates, excluding the current one.
#'     - `poly`: polynomial contrasts, as computed by the `stats::contr.poly()` function.
#'     - `helmert`: Helmert contrasts, as computed by the `stats::contr.helmert()` function. Contrast 2nd level to the first, 3rd to the average of the first two, and so on.
#'     - `trt_vs_ctrl`: difference between the mean of estimates (except the first) and the first estimate.
#'     - `I(fun(x))`: custom function to manipulate the vector of estimates `x`. The function `fun()` can return multiple (potentially named) estimates.
#'   + `group` (optional)
#'     - Column name of `newdata`. Conduct hypothesis tests withing subsets of the data.
#'   + Examples:
#'      + `~ poly`
#'      + `~ sequential | groupid`
#'      + `~ reference`
#'      + `ratio ~ pairwise`
#'      + `difference ~ pairwise | groupid`
#'      + `~ I(x - mean(x)) | groupid`
#'      + `~ I(\(x) c(a = x[1], b = mean(x[2:3]))) | groupid`
#' + Matrix or Vector: Each column is a vector of weights. The the output is the dot product between these vectors of weights and the vector of estimates. The matrix can have column names to label the estimates.
#' + Function:
#'   - Accepts an argument `x`: object produced by a `marginaleffects` function or a data frame with column `rowid` and `estimate`
#'   - Returns a data frame with columns `term` and `estimate` (mandatory) and `rowid` (optional).
#'   - The function can also accept optional input arguments: `newdata`, `by`, `draws`.
#'   - This function approach will not work for Bayesian models or with bootstrapping. In those cases, it is easy to use `get_draws()` to extract and manipulate the draws directly.
#' + See the Examples section below and the vignette: [https://marginaleffects.com/chapters/hypothesis.html](https://marginaleffects.com/chapters/hypothesis.html)
#' @param df Degrees of freedom used to compute p values and confidence intervals. 
#'   - A single numeric value between 1 and `Inf`, or a numeric vector with length equal to the number of rows in the output. When `df` is `Inf`, the normal distribution is used. When `df` is finite, the `t` distribution is used. 
#'   - "residual": Calls [insight::get_df] to extract degrees of freedom from the model automatically.
#'   - "satterthwaite" or "kenward-roger": Use the Satterthwaite or Kenward-Roger approximation to compute degrees of freedom in mixed effects models.
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
#' @template references
#' @template deltamethod
#' @template model_specific_arguments
#' @template bayesian
#' @template equivalence
#' @template type
#' @template parallel
#' @template order_of_operations
#' @template options
#' @template return
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
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
#'   newdata = datagrid(
#'     hp = c(100, 110),
#'     grid_type = "counterfactual"))
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
#'   mod,
#'   newdata = "mean",
#'   hypothesis = "wt = drat")
#'
#' # same hypothesis test using row indices
#' slopes(
#'   mod,
#'   newdata = "mean",
#'   hypothesis = "b1 - b2 = 0")
#'
#' # same hypothesis test using numeric vector of weights
#' slopes(
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
#' colnames(lc) <- c("Contrast A", "Contrast B")
#' slopes(
#'   mod,
#'   newdata = "mean",
#'   hypothesis = lc)
#'
#' @export
slopes <- function(
    model,
    newdata = NULL,
    variables = NULL,
    type = NULL,
    by = FALSE,
    vcov = TRUE,
    conf_level = 0.95,
    slope = "dydx",
    wts = FALSE,
    hypothesis = NULL,
    equivalence = NULL,
    df = Inf,
    eps = NULL,
    numderiv = "fdforward",
    ...
) {
    call_attr <- construct_call(model, "slopes")

    # very early, before any use of newdata
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    # scall <- rlang::enquo(newdata)
    # newdata <- sanitize_newdata_call(scall, newdata, model, by = by)

    # build call: match.call() doesn't work well in *apply()
    # call_attr <- c(
    #     list(
    #         name = "slopes",
    #         model = model,
    #         newdata = newdata,
    #         variables = variables,
    #         type = type,
    #         vcov = vcov,
    #         by = by,
    #         conf_level = conf_level,
    #         slope = slope,
    #         wts = wts,
    #         hypothesis = hypothesis,
    #         df = df,
    #         eps = eps),
    #     list(...))
    # call_attr <- do.call("call", call_attr)

    # slopes() does not support a named list of variables like comparisons()
    checkmate::assert_character(variables, null.ok = TRUE)

    # slope
    valid <- c(
        "dydx",
        "eyex",
        "eydx",
        "dyex",
        "dydxavg",
        "eyexavg",
        "eydxavg",
        "dyexavg"
    )
    checkmate::assert_choice(slope, choices = valid)

    # sanity checks and pre-processing
    model <- sanitize_model(
        model = model,
        wts = wts,
        vcov = vcov,
        by = by,
        calling_function = "slopes",
        ...
    )
    sanity_dots(model = model, calling_function = "slopes", ...)
    type <- sanitize_type(model = model, type = type, calling_function = "slopes")

    ############### sanity checks are over

    call_attr_c <- call_attr
    call_attr_c[[1L]] <- quote(marginaleffects::comparisons)
    call_attr_c[["model"]] <- model
    call_attr_c[["type"]] <- type
    call_attr_c[["comparison"]] <- slope
    call_attr_c[["cross"]] <- FALSE
    call_attr_c[["internal_call"]] <- TRUE
    call_attr_c[["slope"]] <- NULL

    # out <- comparisons(
    #     model,
    #     newdata = newdata,
    #     variables = variables,
    #     vcov = vcov,
    #     conf_level = conf_level,
    #     type = type,
    #     wts = wts,
    #     hypothesis = hypothesis,
    #     equivalence = equivalence,
    #     df = df,
    #     by = by,
    #     eps = eps,
    #     numderiv = numderiv,
    #     comparison = slope,
    #     cross = FALSE,
    #     # secret arguments
    #     internal_call = TRUE,
    #     ...)

    out <- eval.parent(call_attr_c)

    if (!is.null(attr(out, "call"))) {
        attr(out, "call") <- call_attr
    }

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
avg_slopes <- function(
    model,
    newdata = NULL,
    variables = NULL,
    type = NULL,
    by = TRUE,
    vcov = TRUE,
    conf_level = 0.95,
    slope = "dydx",
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
    # should probably not be nested too deeply in the call stack since we eval.parent() (not sure about this)
    # scall <- rlang::enquo(newdata)
    # newdata <- sanitize_newdata_call(scall, newdata, model, by = by)

    #Construct comparisons() call
    call_attr <- construct_call(model, "slopes")

    out <- eval.parent(call_attr)

    # out <- slopes(
    #     model = model,
    #     newdata = newdata,
    #     variables = variables,
    #     type = type,
    #     vcov = vcov,
    #     conf_level = conf_level,
    #     by = by,
    #     slope = slope,
    #     wts = wts,
    #     hypothesis = hypothesis,
    #     equivalence = equivalence,
    #     df = df,
    #     eps = eps,
    #     numderiv = numderiv,
    #     ...)

    return(out)
}
