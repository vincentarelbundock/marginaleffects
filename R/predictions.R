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
#' + Warning: Please avoid modifying your dataset between fitting the model and calling a `marginaleffects` function. This can sometimes lead to unexpected results.
#' + `NULL` (default): Unit-level predictions for each observed value in the dataset (empirical distribution). The dataset is retrieved using [insight::get_data()], which tries to extract data from the environment. This may produce unexpected results if the original data frame has been altered since fitting the model.
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
#' @param byfun A function such as `mean()` or `sum()` used to aggregate
#' estimates within the subgroups defined by the `by` argument. `NULL` uses the
#' `mean()` function. Must accept a numeric vector and return a single numeric
#' value. This is sometimes used to take the sum or mean of predicted
#' probabilities across outcome or predictor
#' levels. See examples section.
#' @param type string indicates the type (scale) of the predictions used to
#' compute contrasts or slopes. This can differ based on the model
#' type, but will typically be a string such as: "response", "link", "probs",
#' or "zero". When an unsupported string is entered, the model-specific list of
#' acceptable values is returned in an error message. When `type` is `NULL`, the
#' first entry in the error message is used by default.
#' @param transform A function applied to unit-level adjusted predictions and confidence intervals just before the function returns results. For bayesian models, this function is applied to individual draws from the posterior distribution, before computing summaries.
#'
#' @template references
#' @template deltamethod
#' @template model_specific_arguments
#' @template bayesian
#' @template equivalence
#' @template type
#' @template order_of_operations
#' @template parallel
#' @template options
#' @template return
#' @examplesIf interactive() || isTRUE(Sys.getenv("R_DOC_BUILD") == "true")
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
#' predictions(mod, newdata = "mean", byfun = sum, by = by)
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
    byfun = NULL,
    wts = FALSE,
    transform = NULL,
    hypothesis = NULL,
    equivalence = NULL,
    df = Inf,
    numderiv = "fdforward",
    ...
) {
    methods <- c("rsample", "boot", "fwb", "simulation")

    if (isTRUE(checkmate::check_choice(vcov, methods))) {
        inferences_method <- vcov
        vcov <- FALSE
    } else {
        inferences_method <- NULL
    }

    call_attr <- construct_call(model, "predictions")
    
    # Create mfx object early and populate as objects become available
    mfx <- new_marginaleffects_internal(call = call_attr)

    # model sanity checks
    mfx <- sanitize_model(mfx, model, newdata = newdata, wts = wts, vcov = vcov, by = by, ...)
    model <- mfx@model

    dots <- list(...)
    sanity_dots(model = model, ...)

    # multiple imputation
    if (inherits(model, c("mira", "amest"))) {
        out <- process_imputation(model, call_attr)
        return(out)
    }

    # modeldata
    modeldata <- get_modeldata(
        model,
        additional_variables = by,
        wts = wts
    )
    mfx@modeldata <- modeldata


    # newdata
    scall <- rlang::enquo(newdata)
    mfx <- add_newdata(mfx, scall, newdata = newdata, by = by, wts = wts, byfun = byfun)
    wts <- mfx@wts

    # sanity checks
    numderiv <- sanitize_numderiv(numderiv)
    sanity_by(by, mfx@newdata)
    sanity_reserved(model, mfx@modeldata)

    # if type is NULL, we backtransform if relevant
    # before sanitize_hypothesis()
    link_to_response <- FALSE
    mfx@type <- sanitize_type(
        model = model,
        type = type,
        by = by,
        hypothesis = hypothesis,
        calling_function = "predictions"
    )
    if (identical(mfx@type, "invlink(link)")) {
        # backtransform: yes
        if (is.null(hypothesis)) {
            link_to_response <- TRUE
        # backtransform: no
        } else {
            mfx@type <- "response"
            insight::format_warning(
                'The `type="invlink"` argument is not available unless `hypothesis` is `NULL` or a single number. The value of the `type` argument was changed to "response" automatically. To suppress this warning, use `type="response"` explicitly in your function call.'
            )
        }
    }

    # hypothesis
    tmp <- sanitize_hypothesis(hypothesis)
    list2env(tmp, envir = environment())

    # save the original because it gets converted to a named list, which breaks
    # user-input sanity checks
    transform_original <- transform
    transform <- sanitize_transform(transform)

    conf_level <- sanitize_conf_level(conf_level, ...)
    if (isFALSE(wts) && "marginaleffects_wts_internal" %in% colnames(mfx@newdata)) {
        wts <- "marginaleffects_wts_internal"
    }

    # analogous to comparisons(variables=list(...))
    if (!is.null(variables)) {
        args <- list(
            model = mfx@model,
            newdata = mfx@newdata,
            grid_type = "counterfactual"
        )
        mfx <- add_variables(
            variables = variables,
            mfx = mfx,
            calling_function = "predictions"
        )
        for (v in mfx@variables$conditional) {
            args[[v$name]] <- v$value
        }
        mfx@newdata <- do.call("datagrid", args)
        # the original rowids are no longer valid after averaging et al.
        mfx@newdata[["rowid"]] <- NULL
    }

    character_levels <- attr(mfx@newdata, "newdata_character_levels")

    # trust newdata$rowid
    if (!"rowid" %in% colnames(mfx@newdata)) {
        mfx@newdata[["rowid"]] <- seq_len(nrow(mfx@newdata))
    }

    # pad factors: `model.matrix` breaks when factor levels are missing
    # support `newdata` and assume no padding the `idx` column is necessary for
    # `get_predict` but it breaks binding, so we can't remove it in
    # sanity_newdata and we can't rbind it with padding
    # pad factors: `model.matrix` breaks when factor levels are missing
    if (inherits(model, "mlogit")) {
        padding <- data.frame()
    } else {
        padding <- complete_levels(mfx@newdata, character_levels)
        if (nrow(padding) > 0) {
            mfx@newdata <- rbindlist(list(padding, mfx@newdata))
        }
    }

    if (is.null(by) || isFALSE(by)) {
        vcov_tmp <- vcov
    } else {
        vcov_tmp <- FALSE
    }

    ############### sanity checks are over

    # pre-building the model matrix can speed up repeated predictions
    mfx@newdata <- get_model_matrix_attribute(mfx@model, mfx@newdata)

    # main estimation
    args <- list(
        mfx = mfx,
        type = if (link_to_response) "link" else mfx@type,
        hypothesis = hypothesis,
        wts = wts,
        by = by,
        byfun = byfun
    )

    args <- utils::modifyList(args, dots)
    tmp <- do.call(get_predictions, args)

    hyp_by <- attr(tmp, "hypothesis_function_by")

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
        colnames(tmp) <- c("rowid", "estimate")
        if ("rowidcf" %in% colnames(mfx@newdata)) {
            tmp[["rowidcf"]] <- mfx@newdata[["rowidcf"]]
        }
    }

    # issue #1105: hypothesis may change the meaning of rows, so we don't want to force-merge `newdata`
    if (
        !"rowid" %in% colnames(tmp) &&
            nrow(tmp) == nrow(mfx@newdata) &&
            is.null(hypothesis)
    ) {
        tmp$rowid <- mfx@newdata$rowid
    }

    # degrees of freedom
    mfx <- add_degrees_of_freedom(
        mfx = mfx, 
        df = df, 
        by = by, 
        hypothesis = hypothesis, 
        vcov = vcov)
    if (!is.null(mfx@df) && is.numeric(mfx@df)) {
        tmp$df <- mfx@df
    }

    # bayesian posterior draws
    draws <- attr(tmp, "posterior_draws")

    J <- NULL
    if (!isFALSE(vcov)) {
        mfx@vcov_model <- get_vcov(mfx@model, vcov = vcov, type = mfx@type, ...)

        # Delta method for standard errors
        if (!"std.error" %in% colnames(tmp) && is.null(draws) && isTRUE(checkmate::check_matrix(mfx@vcov_model))) {
            fun <- function(...) {
                get_predictions(..., wts = wts, verbose = FALSE)$estimate
            }
            args <- list(
                mfx = mfx,
                model_perturbed = mfx@model,
                vcov = mfx@vcov_model,
                type = if (link_to_response) "link" else mfx@type,
                FUN = fun,
                hypothesis = hypothesis,
                by = by,
                byfun = byfun,
                numderiv = numderiv,
                calling_function = "predictions"
            )
            args <- utils::modifyList(args, dots)
            se <- do.call(get_se_delta, args)
            if (is.numeric(se) && length(se) == nrow(tmp)) {
                J <- attr(se, "jacobian")
                attr(se, "jacobian") <- NULL
                tmp[["std.error"]] <- se
            }
        }

        # Confidence intervals
        tmp <- get_ci(
            tmp,
            conf_level = conf_level,
            vcov = mfx@vcov_model,
            draws = draws,
            estimate = "estimate",
            hypothesis_null = hypothesis_null,
            hypothesis_direction = hypothesis_direction,
            df = mfx@df,
            model = mfx@model,
            ...
        )
    }

    out <- data.table::data.table(tmp)
    data.table::setDT(mfx@newdata)

    # expensive: only do this inside jacobian if necessary
    if (!inherits(mfx@model, "mclogit")) {
        # weird case. probably a cleaner way but lazy now...
        out <- merge_by_rowid(out, mfx@newdata)
    }

    # save weights as attribute and not column
    marginaleffects_wts_internal <- out[["marginaleffects_wts_internal"]]
    out[["marginaleffects_wts_internal"]] <- NULL

    # bycols
    if (isTRUE(checkmate::check_data_frame(by))) {
        bycols <- setdiff(colnames(by), "by")
    } else {
        bycols <- by
    }

    # WARNING: we cannot sort rows at the end because `get_hypothesis()` is
    # applied in the middle, and it must already be sorted in the final order,
    # otherwise, users cannot know for sure what is going to be the first and
    # second rows, etc.
    out <- sort_columns(out, mfx@newdata, by)

    attr(out, "posterior_draws") <- draws

    # equivalence tests
    out <- equivalence(out, equivalence = equivalence, df = mfx@df, ...)

    # after rename to estimate / after assign draws
    if (isTRUE(link_to_response)) {
        linv <- tryCatch(insight::link_inverse(mfx@model), error = function(e) identity)
        out <- backtransform(out, transform = linv)
    }
    out <- backtransform(out, transform = transform)

    data.table::setDF(out)
    class(out) <- c("predictions", class(out))

    # Global option for lean return object
    lean <- getOption("marginaleffects_lean", default = FALSE)

    # Only add (potentially large) attributes if lean is FALSE
    # extra attributes needed for print method, even with lean return object
    attr(out, "conf_level") <- conf_level
    attr(out, "by") <- by
    attr(out, "lean") <- lean
    attr(out, "type") <- mfx@type
    if (isTRUE(lean)) {
        for (a in setdiff(
            names(attributes(out)),
            c("names", "row.names", "class")
        )) {
            attr(out, a) <- NULL
        }
    } else {
        # other attributes
        attr(out, "newdata") <- mfx@newdata
        attr(out, "call") <- mfx@call
        attr(out, "model_type") <- class(mfx@model)[1]
        attr(out, "model") <- mfx@model
        attr(out, "jacobian") <- J
        attr(out, "vcov") <- mfx@vcov_model
        attr(out, "weights") <- marginaleffects_wts_internal
        attr(out, "transform") <- transform[[1]]
        attr(out, "hypothesis_by") <- hyp_by
        attr(out, "mfx") <- mfx

        if (inherits(mfx@model, "brmsfit")) {
            insight::check_if_installed("brms")
            attr(out, "nchains") <- brms::nchains(mfx@model)
        }
    }

    if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
        out$group <- NULL
    }

    if (!is.null(inferences_method)) {
        out <- inferences(out, method = inferences_method)
    }

    return(out)
}


# wrapper used only for standard_error_delta
get_predictions <- function(
    mfx,
    type,
    model_perturbed = NULL, # important for perturbed model
    by = NULL,
    byfun = byfun,
    hypothesis = NULL,
    verbose = TRUE,
    wts = FALSE,
    hi = NULL, # sink hole for shared comparisons/predictions call
    lo = NULL, # sink hole
    original = NULL, # sink hole
    ...
) {

    newdata <- mfx@newdata

    # sometimes we want the perturbed coefficients model supplied by get_se_delta().
    if (is.null(model_perturbed)) {
        model <- mfx@model
    } else {
        model <- model_perturbed
    }

    out <- myTryCatch(get_predict(
        model,
        newdata = newdata,
        type = type,
        ...
    ))

    if (inherits(out$value, "data.frame")) {
        out <- out$value
    } else {
        # tidymodels
        if (
            inherits(out$error, "rlang_error") &&
                isTRUE(grepl("the object should be", out$error$message))
        ) {
            insight::format_error(out$error$message)
        }

        msg <- "Unable to compute predicted values with this model. You can try to supply a different dataset to the `newdata` argument."
        if (!is.null(out$error)) {
            msg <- c(paste(msg, "This error was also raised:"), "", out$error$message)
        }
        if (inherits(out$value, "try-error")) {
            msg <- c(
                paste(msg, "", "This error was also raised:"),
                "",
                as.character(out$value)
            )
        }
        msg <- c(
            msg,
            "",
            "Bug Tracker: https://github.com/vincentarelbundock/marginaleffects/issues"
        )
        insight::format_error(msg)
    }

    if (
        !"rowid" %in% colnames(out) &&
            "rowid" %in% colnames(mfx@newdata) &&
            nrow(out) == nrow(mfx@newdata)
    ) {
        out$rowid <- mfx@newdata$rowid
    }

    # extract attributes before setDT
    draws <- attr(out, "posterior_draws")

    data.table::setDT(out)

    # unpad factors before averaging
    # trust `newdata` rowid more than `out` because sometimes `get_predict()` will add a positive index even on padded data
    # HACK: the padding indexing rowid code is still a mess
    # Do not merge `newdata` with `hypothesis`, because it may have the same
    # number of rows but represent different quantities
    if (
        "rowid" %in%
            colnames(newdata) &&
            nrow(newdata) == nrow(out) &&
            is.null(hypothesis)
    ) {
        out$rowid <- newdata$rowid
    }
    # unpad
    if ("rowid" %in% colnames(out)) {
        draws <- draws[out$rowid > 0, , drop = FALSE]
    }
    if ("rowid" %in% colnames(out)) {
        out <- out[out$rowid > 0, , drop = FALSE]
    }
    if ("rowid" %in% colnames(newdata)) {
        newdata <- newdata[newdata$rowid > 0, , drop = FALSE]
    }

    # expensive: only do this inside the jacobian if necessary
    if (
        !isFALSE(wts) ||
            !isTRUE(checkmate::check_flag(by, null.ok = TRUE)) ||
            inherits(model, "mclogit")
    ) {
        # not sure why sorting is so finicky here
        out <- merge_by_rowid(out, newdata)
    }

    # by: auto group
    if (isTRUE(checkmate::check_character(by))) {
        by <- intersect(c("group", by), colnames(out))
    }

    # averaging by groups
    out <- get_by(
        out,
        draws = draws,
        newdata = newdata,
        by = by,
        byfun = byfun,
        verbose = verbose,
        ...
    )

    draws <- attr(out, "posterior_draws")

    # hypothesis tests using the delta method
    out <- get_hypothesis(
        out,
        hypothesis = hypothesis,
        by = by,
        newdata = newdata,
        draws = draws
    )


    return(out)
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
    byfun = NULL,
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
    # scall <- rlang::enquo(newdata)
    # newdata <- sanitize_newdata_call(scall, newdata, model, by = by)

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

    out <- eval.parent(call_attr)

    return(out)
}
