#' Predictions
#'
#' @description
#' Outcome predicted by a fitted model on a specified scale for a given combination of values of the predictor variables, such as their observed values, their means, or factor levels (a.k.a. "reference grid").
#'
#' * `predictions()`: unit-level (conditional) estimates.
#' * `avg_predictions()`: average (marginal) estimates.
#'
#' The `newdata` argument and the `datagrid()` function can be used to control where statistics are evaluated in the predictor space: "at observed values", "at the mean", "at representative values", etc.
#'
#' See the predictions vignette and package website for worked examples and case studies:

#' * <https://marginaleffects.com/chapters/predictions.html>
#' * <https://marginaleffects.com/>
#'
#' @rdname predictions
#' @param model Model object
#' @param variables Counterfactual variables.
#' * Output:
#'   - `predictions()`: The entire dataset is replicated once for each unique combination of `variables`, and predictions are made.
#'   - `avg_predictions()`: The entire dataset is replicated, predictions are made, and they are marginalized by `variables` categories.
#'   - Warning: This can be expensive in large datasets.
#'   - Warning: Users who need "conditional" predictions should use the `newdata` argument instead of `variables`.
#' * Input:
#'   - `NULL`: computes one prediction per row of `newdata`
#'   - Character vector: the dataset is replicated once of every combination of unique values of the variables identified in `variables`.
#'   - Named list: names identify the subset of variables of interest and their values. For numeric variables, the `variables` argument supports functions and string shortcuts:
#'     + A function which returns a numeric value
#'     + Numeric vector: Contrast between the 2nd element and the 1st element of the `x` vector.
#'     + "iqr": Contrast across the interquartile range of the regressor.
#'     + "sd": Contrast across one standard deviation around the regressor mean.
#'     + "2sd": Contrast across two standard deviations around the regressor mean.
#'     + "minmax": Contrast between the maximum and the minimum values of the regressor.
#'     + "threenum": mean and 1 standard deviation on both sides
#'     + "fivenum": Tukey's five numbers
#' @param newdata Grid of predictor values at which we evaluate predictions.
#' + Warning: When `newdata` is `NULL`, the data is retrieved using [get_modeldata()], which may have to extract it from the modeling environment. This can produce unexpected results if the data has been modified, or when called inside `lapply()`, Shiny apps, or nested functions. To avoid potential issues, consider using [set_modeldata()] to attach the training data to the model object explicitly.
#' + `NULL` (default): Unit-level predictions for each observed value in the dataset (empirical distribution). The dataset is retrieved using [get_modeldata()].
#' + string:
#'   - "mean": Predictions evaluated when each predictor is held at its mean or mode.
#'   - "median": Predictions evaluated when each predictor is held at its median or mode.
#'   - "balanced": Predictions evaluated on a balanced grid with every combination of categories and numeric variables held at their means.
#'   - "tukey": Predictions evaluated at Tukey's 5 numbers.
#'   - "grid": Predictions evaluated on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid()] documentation.
#' + [subset()] call with a single argument to select a subset of the dataset used to fit the model, ex: `newdata = subset(treatment == 1)`
#' + [dplyr::filter()] call with a single argument to select a subset of the dataset used to fit the model, ex: `newdata = filter(treatment == 1)`
#' @param type string indicates the type (scale) of the predictions used to
#' compute contrasts or slopes. This can differ based on the model
#' type, but will typically be a string such as: "response", "link", "probs",
#' or "zero". When an unsupported string is entered, the model-specific list of
#' acceptable values is returned in an error message. When `type` is `NULL`, the
#' first entry in the error message is used by default. See the Type section in the documentation below.
#' @param transform A function applied to unit-level adjusted predictions and confidence intervals just before the function returns results. For bayesian models, this function is applied to individual draws from the posterior distribution, before computing summaries.
#'
#' @template references
#' @template deltamethod
#' @template model_specific_arguments
#' @template bayesian
#' @template equivalence
#' @template order_of_operations
#' @template parallel
#' @template options
#' @template return
#' @template type
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
#' library("marginaleffects")
#' # Adjusted Prediction for every row of the original dataset
#' mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
#' pred <- predictions(mod)
#' head(pred)
#'
#' # Adjusted Predictions at User-Specified Values of the Regressors
#' predictions(mod, newdata = datagrid(hp = c(100, 120), cyl = 4))
#'
#' m <- lm(mpg ~ hp + drat + factor(cyl) + factor(am), data = mtcars)
#' predictions(m, newdata = datagrid(FUN_factor = unique, FUN_numeric = median))
#'
#' # Average Adjusted Predictions (AAP)
#' library(dplyr)
#' mod <- lm(mpg ~ hp * am * vs, mtcars)
#'
#' avg_predictions(mod)
#'
#' predictions(mod, by = "am")
#'
#' # Conditional Adjusted Predictions
#' plot_predictions(mod, condition = "hp")
#'
#' # Counterfactual predictions with the `variables` argument
#' # the `mtcars` dataset has 32 rows
#'
#' mod <- lm(mpg ~ hp + am, data = mtcars)
#' p <- predictions(mod)
#' head(p)
#' nrow(p)
#'
#' # average counterfactual predictions
#' avg_predictions(mod, variables = "am")
#'
#' # counterfactual predictions obtained by replicating the entire for different
#' # values of the predictors
#' p <- predictions(mod, variables = list(hp = c(90, 110)))
#' nrow(p)
#'
#'
#' # hypothesis test: is the prediction in the 1st row equal to the prediction in the 2nd row
#' mod <- lm(mpg ~ wt + drat, data = mtcars)
#'
#' predictions(
#'   mod,
#'   newdata = datagrid(wt = 2:3),
#'   hypothesis = "b1 = b2")
#'
#' # same hypothesis test using row indices
#' predictions(
#'   mod,
#'   newdata = datagrid(wt = 2:3),
#'   hypothesis = "b1 - b2 = 0")
#'
#' # same hypothesis test using numeric vector of weights
#' predictions(
#'   mod,
#'   newdata = datagrid(wt = 2:3),
#'   hypothesis = c(1, -1))
#'
#' # two custom contrasts using a matrix of weights
#' lc <- matrix(
#'   c(
#'     1, -1,
#'     2, 3),
#'   ncol = 2)
#' predictions(
#'   mod,
#'   newdata = datagrid(wt = 2:3),
#'   hypothesis = lc)
#'
#'
#' # `by` argument
#' mod <- lm(mpg ~ hp * am * vs, data = mtcars)
#' predictions(mod, by = c("am", "vs"))
#'
#' library(nnet)
#' nom <- multinom(factor(gear) ~ mpg + am * vs, data = mtcars, trace = FALSE)
#'
#' # first 5 raw predictions
#' p <- predictions(nom, type = "probs")
#' head(p)
#'
#' # average predictions
#' avg_predictions(nom, type = "probs", by = "group")
#'
#' by <- data.frame(
#'   group = c("3", "4", "5"),
#'   by = c("3,4", "3,4", "5"))
#'
#' predictions(nom, type = "probs", by = by)
#'
#' # sum of predicted probabilities for combined response levels
#' mod <- multinom(factor(cyl) ~ mpg + am, data = mtcars, trace = FALSE)
#' by <- data.frame(
#'   by = c("4,6", "4,6", "8"),
#'   group = as.character(c(4, 6, 8)))
#' hyp <- function(x) {
#'   out <- aggregate(estimate ~ by, merge(x, by, by = "group"), sum)
#'   names(out)[1] <- "hypothesis"
#'   out
#' }
#' predictions(mod, newdata = "mean", hypothesis = hyp)
#'
#' @inheritParams slopes
#' @inheritParams comparisons
#' @export
predictions <- function(
    model,
    newdata = NULL,
    variables = NULL,
    vcov = TRUE,
    conf_level = 0.95,
    type = NULL,
    by = FALSE,
    wts = FALSE,
    transform = NULL,
    hypothesis = NULL,
    equivalence = NULL,
    df = Inf,
    numderiv = "fdforward",
    ...
) {
    # init
    mfx <- marginaleffects_init(
        model = model,
        calling_function = "predictions",
        newdata = newdata,
        wts = wts,
        vcov = vcov,
        by = by,
        env = environment(),
        ...
    )

    # newdata
    scall <- rlang::enquo(newdata)
    mfx <- add_newdata(mfx, scall, newdata = newdata, by = by, wts = wts)

    # inferences() dispatch
    inferences_dispatch <- sanitize_inferences_method(vcov)
    vcov <- inferences_dispatch$vcov
    inferences_method <- inferences_dispatch$method
    unconditional_df <- if (missing(df)) "residual" else df
    vcov <- sanitize_unconditional_vcov_request(
        vcov,
        mfx,
        df = unconditional_df,
        df_supplied = !missing(df)
    )
    unconditional <- is_unconditional_vcov(vcov)

    dots <- list(...)
    sanity_dots(model = mfx@model, ...)

    # multiple imputation
    if (inherits(mfx@model, c("mira", "amest"))) {
        out <- process_imputation(mfx)
        return(out)
    }


    # sanity checks
    mfx <- add_numderiv(mfx, numderiv)
    mfx <- add_by(mfx, by)
    sanity_reserved(mfx)

    # if type is NULL, we backtransform if relevant
    # before add_hypothesis()
    link_to_response <- FALSE
    mfx@type <- sanitize_type(
        model = mfx@model,
        type = type,
        by = by,
        hypothesis = hypothesis,
        calling_function = mfx@calling_function
    )
    if (identical(mfx@type, "invlink(link)")) {
        # backtransform: yes
        if (is.null(hypothesis)) {
            link_to_response <- TRUE
            # backtransform: no
        } else {
            mfx@type <- "response"
            warn_sprintf(
                'The `type="invlink"` argument is not available unless `hypothesis` is `NULL` or a single number. The value of the `type` argument was changed to "response" automatically. To suppress this warning, use `type="response"` explicitly in your function call.'
            )
        }
    }

    mfx <- add_hypothesis(mfx, hypothesis)

    transform <- sanitize_transform(transform)

    mfx@conf_level <- sanitize_conf_level(conf_level, ...)

    mfx <- prediction_prepare_newdata(mfx, variables = variables)
    unpadded_newdata <- mfx@newdata # for degrees of freedom
    mfx@newdata <- pad(mfx@model, mfx@newdata)
    mfx@newdata <- add_model_matrix_attribute(mfx)

    ############### sanity checks are over

    prediction_type <- if (link_to_response) "link" else mfx@type

    # main estimation
    args <- list(
        mfx = mfx,
        type = prediction_type,
        hypothesis = mfx@hypothesis,
        by = by
    )

    args <- utils::modifyList(args, dots)
    built <- do_call(prediction_plan_build, args)
    tmp <- built$cmp

    # hypothesis formula names are attached in by()
    mfx@variable_names_by <- unique(c(
        mfx@variable_names_by,
        attr(tmp, "hypothesis_function_by")))

    # two cases when tmp is a data.frame
    # get_predict gets us rowid with the original rows
    if (inherits(tmp, "data.frame")) {
        setnames(
            tmp,
            old = c("Predicted", "SE", "CI_low", "CI_high"),
            new = c("estimate", "std.error", "conf.low", "conf.high"),
            skip_absent = TRUE
        )
    } else {
        tmp <- data.frame(mfx@newdata$rowid, mfx@type, tmp)
        colnames(tmp) <- c("rowid", "type", "estimate")
        if ("rowidcf" %in% colnames(mfx@newdata)) {
            tmp[["rowidcf"]] <- mfx@newdata[["rowidcf"]]
        }
    }

    # bayesian posterior draws
    mfx@draws <- attr(tmp, "posterior_draws")

    if (!unconditional && !isFALSE(vcov)) {
        mfx@vcov_type <- get_vcov_label(vcov)
        mfx@vcov_model <- get_vcov(mfx@model, vcov = vcov, type = prediction_type, ...)
    }

    se <- plan_std_error(
        built = built,
        mfx = mfx,
        estimates = tmp,
        type = prediction_type,
        vcov = vcov,
        dots = dots,
        variables = mfx@variables
    )
    mfx <- se$mfx
    tmp <- se$estimates

    # Common path for both autodiff and fallback

    mfx <- add_degrees_of_freedom(
        mfx = mfx,
        df = if (unconditional) Inf else df,
        by = by,
        hypothesis = mfx@hypothesis,
        vcov = vcov,
        newdata = unpadded_newdata
    )
    if (unconditional) {
        mfx@df <- vcov$df
    }
    if (unconditional && unconditional_df_all_infinite(vcov$df)) {
        if ("df" %in% colnames(tmp)) {
            tmp$df <- NULL
        }
    } else if (!is.null(mfx@df) && is.numeric(mfx@df)) {
        tmp$df <- mfx@df
    }

    out <- data.table::data.table(tmp)
    data.table::setDT(mfx@newdata)

    out <- merge_original_data(out, mfx@newdata, by = by)

    linv <- if (isTRUE(link_to_response)) {
        tryCatch(insight::link_inverse(mfx@model), error = function(e) identity)
    } else {
        NULL
    }

    conf_int <- (!isFALSE(vcov) || unconditional) &&
        ("std.error" %in% colnames(out) || !is.null(mfx@draws))

    return(finalize_estimates(
        out = out,
        mfx = mfx,
        by = by,
        transform = transform,
        equivalence = equivalence,
        class_name = "predictions",
        inferences_method = inferences_method,
        drop_group = TRUE,
        conf_int = conf_int,
        pre_transform = linv,
        ...
    ))
}


#' Average predictions
#' @describeIn predictions Average predictions
#' @export
#'
avg_predictions <- function(
    model,
    newdata = NULL,
    variables = NULL,
    vcov = TRUE,
    conf_level = 0.95,
    type = NULL,
    by = TRUE,
    wts = FALSE,
    transform = NULL,
    hypothesis = NULL,
    equivalence = NULL,
    df = Inf,
    numderiv = "fdforward",
    ...
) {
    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`

    # group by focal variable automatically unless otherwise stated
    if (isTRUE(by)) {
        if (isTRUE(checkmate::check_character(variables))) {
            by <- variables
        } else if (isTRUE(checkmate::check_list(variables, names = "named"))) {
            by <- names(variables)
        }
    }

    #Construct predictions() call
    call_attr <- construct_call(model, "predictions")
    call_attr[["by"]] <- by
    if (missing(df)) {
        call_attr[["df"]] <- NULL
    }

    out <- eval.parent(call_attr)

    return(out)
}
