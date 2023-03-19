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

#' * <https://vincentarelbundock.github.io/marginaleffects/articles/predictions.html>
#' * <https://vincentarelbundock.github.io/marginaleffects/>
#'
#' @rdname predictions
#' @details
#' For `glm()` or `gam::gam()` models with `type=NULL` (the default), `predictions()` first predicts on the link scale, and then backtransforms the estimates and confidence intervals. This implies that the `estimate` produced by `avg_predictions()` will not be exactly equal to the average of the `estimate` column produced by `predictions()`. Users can circumvent this behavior and average predictions directly on the response scale by setting `type="response"` explicitly. With `type="response"`, the intervals are symmetric and may have undesirable properties (e.g., stretching beyond the `[0,1]` bounds for a binary outcome regression).
#' 
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
#' + `NULL` (default): Predictions for each observed value in the original dataset.
#' + data frame: Predictions for each row of the `newdata` data frame.
#' + string:
#'   - "mean": Predictions at the Mean. Predictions when each predictor is held at its mean or mode.
#'   - "median": Predictions at the Median. Predictions when each predictor is held at its median or mode.
#'   - "marginalmeans": Predictions at Marginal Means. See Details section below.
#'   - "tukey": Predictions at Tukey's 5 numbers.
#'   - "grid": Predictions on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).
#' + [datagrid()] call to specify a custom grid of regressors. For example:
#'   - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'   - See the Examples section and the [datagrid()] documentation.
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
#' default value is used. This default is the first model-related row in
#' the `marginaleffects:::type_dictionary` dataframe. See the details section for a note on backtransformation.
#' @param transform A function applied to unit-level adjusted predictions and confidence intervals just before the function returns results. For bayesian models, this function is applied to individual draws from the posterior distribution, before computing summaries.
#'
#' @template deltamethod
#' @template model_specific_arguments
#' @template bayesian
#' @template equivalence
#'
#' @return A `data.frame` with one row per observation and several columns:
#' * `rowid`: row number of the `newdata` data frame
#' * `type`: prediction type, as defined by the `type` argument
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `estimate`: predicted outcome
#' * `std.error`: standard errors computed using the delta method.
#' * `conf.low`: lower bound of the confidence interval (or equal-tailed interval for bayesian models)
#' * `conf.high`: upper bound of the confidence interval (or equal-tailed interval for bayesian models)
#' * `p.value`: p value associated to the `estimate` column. The null is determined by the `hypothesis` argument (0 by default), and p values are computed before applying the `transform` argument. For models of class `feglm`, `Gam`, `glm` and `negbin`, p values are computed on the link scale by default unless the `type` argument is specified explicitly.
#'
#' See `?print.marginaleffects` for printing options.
#'
#' @examples
#' \dontrun{
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
#'     mod,
#'     newdata = datagrid(wt = 2:3),
#'     hypothesis = "b1 = b2")
#'
#' # same hypothesis test using row indices
#' predictions(
#'     mod,
#'     newdata = datagrid(wt = 2:3),
#'     hypothesis = "b1 - b2 = 0")
#'
#' # same hypothesis test using numeric vector of weights
#' predictions(
#'     mod,
#'     newdata = datagrid(wt = 2:3),
#'     hypothesis = c(1, -1))
#'
#' # two custom contrasts using a matrix of weights
#' lc <- matrix(c(
#'     1, -1,
#'     2, 3),
#'     ncol = 2)
#' predictions(
#'     mod,
#'     newdata = datagrid(wt = 2:3),
#'     hypothesis = lc)
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
#' predictions(nom, type = "probs") |> head()
#'
#' # average predictions
#' avg_predictions(nom, type = "probs", by = "group")
#'
#' by <- data.frame(
#'     group = c("3", "4", "5"),
#'     by = c("3,4", "3,4", "5"))
#'
#' predictions(nom, type = "probs", by = by)
#'
#' # sum of predicted probabilities for combined response levels
#' mod <- multinom(factor(cyl) ~ mpg + am, data = mtcars, trace = FALSE)
#' by <- data.frame(
#'     by = c("4,6", "4,6", "8"),
#'     group = as.character(c(4, 6, 8)))
#' predictions(mod, newdata = "mean", byfun = sum, by = by)
#' }
#'
#' @inheritParams slopes
#' @inheritParams comparisons
#' @export
predictions <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        vcov = TRUE,
                        conf_level = 0.95,
                        type = NULL,
                        by = FALSE,
                        byfun = NULL,
                        wts = NULL,
                        transform = NULL,
                        hypothesis = NULL,
                        equivalence = NULL,
                        p_adjust = NULL,
                        df = Inf,
                        ...) {


    dots <- list(...)
    
    # backward compatibility
    if ("transform_post" %in% names(dots)) transform <- dots[["transform_post"]]

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    scall <- substitute(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, model)

    if (!is.null(equivalence) && !is.null(p_adjust)) {
        insight::format_error("The `equivalence` and `p_adjust` arguments cannot be used together.")
    }
    
    
    # is the model supported?
    model <- sanitize_model(
        model = model,
        newdata = newdata,
        wts = wts,
        vcov = vcov,
        calling_function = "predictions",
        ...)

    # build call: match.call() doesn't work well in *apply()
    call_attr <- c(list(
        name = "predictions",
        model = model,
        newdata = newdata,
        variables = variables,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        by = by,
        byfun = byfun,
        wts = wts,
        transform = transform,
        hypothesis = hypothesis,
        df = df),
        list(...))
    call_attr <- do.call("call", call_attr)

    # multiple imputation
    if (inherits(model, "mira")) {
        out <- process_imputation(model, call_attr)
        return(out)
    }

    # extracting modeldata repeatedly is slow.
    # checking dots allows marginalmeans to pass modeldata to predictions.
    if ("modeldata" %in% names(dots)) {
        modeldata <- dots[["modeldata"]]
    } else {
        if (isTRUE(checkmate::check_character(by))) {
            addvar <- by
        } else if (isTRUE(checkmate::check_data_frame(by))) {
            addvar <- colnames(by)
        } else {
            addvar <- FALSE
        }
        modeldata <- get_modeldata(model, additional_variables = addvar)
    }

    # if type is NULL, we backtransform if relevant
    flag1 <- is.null(type)
    flag2 <- is.null(transform)
    flag3 <- isTRUE(class(model)[1] %in% c("glm", "Gam", "negbin"))
    flag4 <- isTRUE(hush(model[["method_type"]]) %in% c("feglm"))
    if (flag1 && flag2 && (flag3 || flag4)) {
        dict <- subset(type_dictionary, class == class(model)[1])$type
        type <- sanitize_type(model = model, type = type)
        linv <- tryCatch(insight::link_inverse(model), error = function(e) NULL)
        if (isTRUE(type == "response") && isTRUE("link" %in% dict) && is.function(linv)) {
            type <- "link"
            transform <- linv
        } else {
            type <- sanitize_type(model = model, type = type)
        }
    } else {
        type <- sanitize_type(model = model, type = type)
    }

    # do not check the model because `insight` supports more models than `marginaleffects`
    # model <- sanitize_model(model)

    # input sanity checks
    checkmate::assert_number(df, lower = 1)

    transform <- sanitize_transform(transform)
    sanity_dots(model = model, ...)
    model <- sanitize_model_specific(
        model = model,
        newdata = newdata,
        vcov = vcov,
        calling_function = "predictions",
        ...)

    tmp <- sanitize_hypothesis(hypothesis, ...)
    hypothesis <- tmp$hypothesis
    hypothesis_null <- tmp$hypothesis_null

    conf_level <- sanitize_conf_level(conf_level, ...)
    newdata <- sanitize_newdata(
        model = model,
        newdata = newdata,
        modeldata = modeldata,
        by = by)

    # after sanitize_newdata
    sanity_by(by, newdata)

    # after sanity_by
    newdata <- dedup_newdata(
        model = model,
        newdata = newdata,
        wts = wts,
        by = by,
        byfun = byfun)
    if (is.null(wts) && "marginaleffects_wts_internal" %in% colnames(newdata)) {
        wts <- "marginaleffects_wts_internal"
    }

    # weights: after sanitize_newdata; before sanitize_variables
    sanity_wts(wts, newdata) # after sanity_newdata
    if (!is.null(wts) && isTRUE(checkmate::check_string(wts))) {
        newdata[["marginaleffects_wts_internal"]] <- newdata[[wts]]
    } else {
        newdata[["marginaleffects_wts_internal"]] <- wts
    }

    # analogous to comparisons(variables=list(...))
    if (!is.null(variables)) {
        args <- list(
            "model" = model,
            "newdata" = newdata,
            "grid_type" = "counterfactual")
        tmp <- sanitize_variables(
            variables = variables,
            model = model,
            newdata = newdata,
            modeldata = modeldata,
            calling_function = "predictions"
            )$conditional
        for (v in tmp) {
            args[[v$name]] <- v$value
        }
        newdata <- do.call("datagrid", args)
        # the original rowids are no longer valid after averaging et al.
        newdata[["rowid"]] <- NULL
    }

    character_levels <- attr(newdata, "newdata_character_levels")


    # trust newdata$rowid
    if (!"rowid" %in% colnames(newdata)) {
        newdata[["rowid"]] <- seq_len(nrow(newdata))
    }

    # mlogit models sometimes returns an `idx` column that is impossible to `rbind`
    if (inherits(model, "mlogit") && inherits(newdata[["idx"]], "idx")) {
        newdata[["idx"]] <- NULL
    }

    # padding destroys `newdata` attributes, so we save them
    newdata_attr_cache <- get_marginaleffects_attributes(newdata, include_regex = "^newdata")

    # mlogit uses an internal index that is very hard to track, so we don't
    # support `newdata` and assume no padding the `idx` column is necessary for
    # `get_predict` but it breaks binding, so we can't remove it in
    # sanity_newdata and we can't rbind it with padding
    # pad factors: `model.matrix` breaks when factor levels are missing
    if (inherits(model, "mlogit")) {
        padding <- data.frame()
    } else {
        padding <- complete_levels(newdata, character_levels)
        if (nrow(padding) > 0) {
            newdata <- rbindlist(list(padding, newdata))
        }
    }

    if (is.null(by) || isFALSE(by)) {
        vcov_tmp <- vcov
    } else {
        vcov_tmp <- FALSE
    }


    ############### sanity checks are over

    # Bootstrap
    out <- inferences_dispatch(
        FUN = predictions,
        model = model, newdata = newdata, vcov = vcov, variables = variables, type = type, by = by,
        conf_level = conf_level,
        byfun = byfun, wts = wts, transform = transform, hypothesis = hypothesis, ...)
    if (!is.null(out)) {
        return(out)
    }
    

    # pre-building the model matrix can speed up repeated predictions
    newdata <- get_model_matrix_attribute(model, newdata)


    # main estimation
    args <- list(
        model = model,
        newdata = newdata,
        type = type,
        hypothesis = hypothesis,
        wts = wts,
        by = by,
        byfun = byfun)

    args <- utils::modifyList(args, dots)
    tmp <- do.call(get_predictions, args)

    # two cases when tmp is a data.frame
    # get_predict gets us rowid with the original rows
    if (inherits(tmp, "data.frame")) {
        setnames(tmp,
                 old = c("Predicted", "SE", "CI_low", "CI_high"),
                 new = c("estimate", "std.error", "conf.low", "conf.high"),
                 skip_absent = TRUE)
    } else {
        tmp <- data.frame(newdata$rowid, type, tmp)
        colnames(tmp) <- c("rowid", "estimate")
        if ("rowidcf" %in% colnames(newdata)) {
            tmp[["rowidcf"]] <- newdata[["rowidcf"]]
        }
    }

    if (!"rowid" %in% colnames(tmp) && nrow(tmp) == nrow(newdata)) {
        tmp$rowid <- newdata$rowid
    }

    # degrees of freedom
    if (isTRUE(vcov == "satterthwaite") || isTRUE(vcov == "kenward-roger")) {
        df <- tryCatch(
            # df_per_observation is an undocumented argument introduced in 0.18.4.7 to preserve backward incompatibility
            insight::get_df(model, data = newdata, type = vcov, df_per_observation = TRUE),
            error = function(e) NULL)
        if (isTRUE(length(df) == nrow(tmp))) {
            tmp$df <- df
        }
    }

    # bayesian posterior draws
    draws <- attr(tmp, "posterior_draws")

    # bayesian: unpad draws (done in get_predictions for frequentist)
    if (!is.null(draws) && "rowid" %in% colnames(tmp)) {
        draws <- draws[tmp$rowid > 0, , drop = FALSE]
    }

    V <- NULL
    J <- NULL
    if (!isFALSE(vcov)) {
        V <- get_vcov(model, vcov = vcov, ...)

        # Delta method
        if (!"std.error" %in% colnames(tmp) && is.null(draws)) {
            if (isTRUE(checkmate::check_matrix(V))) {
                # vcov = FALSE to speed things up
                fun <- function(...) {
                    get_predictions(..., wts = wts, verbose = FALSE)$estimate
                }
                args <- list(
                    model,
                    newdata = newdata,
                    vcov = V,
                    type = type,
                    FUN = fun,
                    J = J,
                    hypothesis = hypothesis,
                    by = by,
                    byfun = byfun)
                args <- utils::modifyList(args, dots)
                se <- do.call(get_se_delta, args)
                if (is.numeric(se) && length(se) == nrow(tmp)) {
                    J <- attr(se, "jacobian")
                    attr(se, "jacobian") <- NULL
                    tmp[["std.error"]] <- se
                }
            }
        }

        tmp <- get_ci(
            tmp,
            conf_level = conf_level,
            vcov = vcov,
            draws = draws,
            estimate = "estimate",
            null_hypothesis = hypothesis_null,
            df = df,
            model = model,
            p_adjust = p_adjust,
            ...)
    }

    out <- data.table::data.table(tmp)
    data.table::setDT(newdata)

    # expensive: only do this inside jacobian if necessary
    if (!inherits(model, "mclogit")) { # weird case. probably a cleaner way but lazy now...
        out <- merge_by_rowid(out, newdata)
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

    # sort rows
    if (is.null(draws)) { # dangerous for bayes to align draws
        idx <- c("term", grep("^rowid|^group$", colnames(out), value = TRUE), bycols)
        idx <- intersect(idx, colnames(out))
        if (length(idx) > 0) data.table::setorderv(out, cols = idx)
    }

    # clean columns
    stubcols <- c(
        "rowid", "rowidcf", "term", "group", "hypothesis",
        bycols,
        "estimate", "std.error", "statistic", "p.value", "conf.low",
        "conf.high", "marginaleffects_wts",
        sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, ..cols]

    attr(out, "posterior_draws") <- draws

    # equivalence tests
    out <- equivalence(out, equivalence = equivalence, df = df, ...)

    # after rename to estimate / after assign draws
    out <- backtransform(out, transform = transform)

    data.table::setDF(out)
    class(out) <- c("predictions", class(out))
    out <- set_marginaleffects_attributes(out, attr_cache = newdata_attr_cache)
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "vcov.type") <- get_vcov_label(vcov)
    attr(out, "jacobian") <- J
    attr(out, "vcov") <- V
    attr(out, "newdata") <- newdata
    attr(out, "weights") <- marginaleffects_wts_internal
    attr(out, "conf_level") <- conf_level
    attr(out, "by") <- by
    attr(out, "call") <- call_attr
    attr(out, "transform_label") <- names(transform)[1]
    attr(out, "transform") <- transform[[1]]
    # save newdata for use in recall()
    attr(out, "newdata") <- newdata

    if (inherits(model, "brmsfit")) {
        insight::check_if_installed("brms")
        attr(out, "nchains") <- brms::nchains(model)
    }

    if (inherits(model, "brmsfit")) {
        insight::check_if_installed("brms")
        attr(out, "nchains") <- brms::nchains(model)
    }

    if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
        out$group <- NULL
    }

    return(out)
}


# wrapper used only for standard_error_delta
get_predictions <- function(model,
                            newdata,
                            type,
                            by = NULL,
                            byfun = byfun,
                            hypothesis = NULL,
                            verbose = TRUE,
                            wts = NULL,
                            ...) {


    out <- myTryCatch(get_predict(
        model,
        newdata = newdata,
        type = type,
        ...))


    if (inherits(out$value, "data.frame")) {
        out <- out$value
    } else {
        msg <- "Unable to compute predicted values with this model. You can try to supply a different dataset to the `newdata` argument."
        if (!is.null(out$error)) {
            msg <- c(paste(msg, "This error was also raised:"), "", out$error$message)
        }
        msg <- c(msg, "", "Bug Tracker: https://github.com/vincentarelbundock/marginaleffects/issues")
        insight::format_error(msg)
    }

    if (!"rowid" %in% colnames(out) && "rowid" %in% colnames(newdata) && nrow(out) == nrow(newdata)) {
        out$rowid <- newdata$rowid
    }

    # extract attributes before setDT
    draws <- attr(out, "posterior_draws")

    data.table::setDT(out)

    # unpad factors before averaging
    # trust `newdata` rowid more than `out` because sometimes `get_predict()` will add a positive index even on padded data
    # HACK: the padding indexing rowid code is still a mess
    if ("rowid" %in% colnames(newdata) && nrow(newdata) == nrow(out)) {
        out$rowid <- newdata$rowid
    }
    if ("rowid" %in% colnames(out)) {
        idx <- out$rowid > 0
        out <- out[idx, drop = FALSE]
        draws <- draws[idx, , drop = FALSE]
    }

    # expensive: only do this inside the jacobian if necessary
    if (!is.null(wts) ||
        !isTRUE(checkmate::check_flag(by, null.ok = TRUE)) ||
        inherits(model, "mclogit")) { # not sure why sorting is so finicky here
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
        ...)

    draws <- attr(out, "posterior_draws")

    # hypothesis tests using the delta method
    out <- get_hypothesis(out, hypothesis = hypothesis, by = by)

    return(out)
}


#' Average predictions
#' @describeIn predictions Average predictions
#' @export
#'
avg_predictions <- function(model,
                            newdata = NULL,
                            variables = NULL,
                            vcov = TRUE,
                            conf_level = 0.95,
                            type = NULL,
                            by = TRUE,
                            byfun = NULL,
                            wts = NULL,
                            transform = NULL,
                            hypothesis = NULL,
                            equivalence = NULL,
                            p_adjust = NULL,
                            df = Inf,
                            ...) {

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    scall <- substitute(newdata)
    newdata <- sanitize_newdata_call(scall, newdata, model)

    # group by focal variable automatically unless otherwise stated
    if (isTRUE(by)) {
        if (isTRUE(checkmate::check_character(variables))) {
            by <- variables
        } else if (isTRUE(checkmate::check_list(variables, names = "named"))) {
            by <- names(variables)
        }
    }

    # Bootstrap
    out <- inferences_dispatch(
        FUN = avg_predictions,
        model = model, newdata = newdata, vcov = vcov, variables = variables, type = type, by = by,
        conf_level = conf_level,
        byfun = byfun, wts = wts, transform = transform, hypothesis = hypothesis, ...)
    if (!is.null(out)) {
        return(out)
    }

    out <- predictions(
        model = model,
        newdata = newdata,
        variables = variables,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        by = by,
        byfun = byfun,
        wts = wts,
        transform = transform,
        hypothesis = hypothesis,
        equivalence = equivalence,
        p_adjust = p_adjust,
        df = df,
        ...)

    return(out)
}
