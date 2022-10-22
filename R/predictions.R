#' Adjusted Predictions
#'
#' Outcome predicted by a fitted model on a specified scale for a given
#' combination of values of the predictor variables, such as their observed
#' values, their means, or factor levels (a.k.a. "reference grid"). The
#' `tidy()` and `summary()` functions can be used to aggregate the output of
#' `predictions()`. To learn more, read the predictions vignette, visit the
#' package website, or scroll down this page for a full list of vignettes:
#' * <https://vincentarelbundock.github.io/marginaleffects/articles/predictions.html>
#' * <https://vincentarelbundock.github.io/marginaleffects/>
#'
#' @section Vignettes and documentation:
#'
#' ```{r child = "vignettes/toc.Rmd"}
#' ```
#'
#' @details
#' The `newdata` argument, the `tidy()` function, and `datagrid()` function can be used to control the kind of predictions to report:
#' 
#' * Average Predictions
#' * Predictions at the Mean
#' * Predictions at User-Specified values (aka Predictions at Representative values).
#'
#' When possible, `predictions()` delegates the computation of confidence
#' intervals to the `insight::get_predicted()` function, which uses back
#' transformation to produce adequate confidence intervals on the scale
#' specified by the `type` argument. When this is not possible, `predictions()`
#' uses the Delta Method to compute standard errors around adjusted
#' predictions, and builds symmetric confidence intervals. These naive symmetric
#' intervals may not always be appropriate. For instance, they may stretch beyond
#' the bounds of a binary response variables.
#'
#' @inheritParams marginaleffects
#' @param model Model object
#' @param variables `NULL`, character vector, or named list. The subset of variables to use for creating a counterfactual grid of predictions. The entire dataset replicated for each unique combination of the variables in this list. See the Examples section below.
#' * Warning: This can use a lot of memory if there are many variables and values, and when the dataset is large.
#' * `NULL`: computes one prediction per row of `newdata`
#' * Named list: names identify the subset of variables of interest and their values. For numeric variables, the `variables` argument supports functions and string shortcuts:
#'   - A function which returns a numeric value
#'   - Numeric vector: Contrast between the 2nd element and the 1st element of the `x` vector.
#'   - "iqr": Contrast across the interquartile range of the regressor.
#'   - "sd": Contrast across one standard deviation around the regressor mean.
#'   - "2sd": Contrast across two standard deviations around the regressor mean.
#'   - "minmax": Contrast between the maximum and the minimum values of the regressor.
#'   - "threenum": mean and 1 standard deviation on both sides
#'   - "fivenum": Tukey's five numbers
#' #' @param newdata `NULL`, data frame, string, or `datagrid()` call. Determines the grid of predictors on which we make predictions.
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
#' @param transform_post (experimental) A function applied to unit-level adjusted predictions and confidence intervals just before the function returns results. For bayesian models, this function is applied to individual draws from the posterior distribution, before computing summaries.
#'
#' @template model_specific_arguments
#'
#' @return A `data.frame` with one row per observation and several columns:
#' * `rowid`: row number of the `newdata` data frame
#' * `type`: prediction type, as defined by the `type` argument
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `predicted`: predicted outcome
#' * `std.error`: standard errors computed by the `insight::get_predicted` function or, if unavailable, via `marginaleffects` delta method functionality.
#' * `conf.low`: lower bound of the confidence interval (or equal-tailed interval for bayesian models)
#' * `conf.high`: upper bound of the confidence interval (or equal-tailed interval for bayesian models)
#' @examples
#' # Adjusted Prediction for every row of the original dataset
#' mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
#' pred <- predictions(mod)
#' head(pred)
#'
#' # Adjusted Predictions at User-Specified Values of the Regressors
#' predictions(mod, newdata = datagrid(hp = c(100, 120), cyl = 4))
#'
#' # Average Adjusted Predictions (AAP)
#' library(dplyr)
#' mod <- lm(mpg ~ hp * am * vs, mtcars)
#'
#' pred <- predictions(mod)
#' summary(pred)
#' 
#' predictions(mod, by = "am")
#'
#' # Conditional Adjusted Predictions
#' plot_cap(mod, condition = "hp")
#'
#' # Counterfactual predictions with the `variables` argument
#' # the `mtcars` dataset has 32 rows
#' 
#' mod <- lm(mpg ~ hp + am, data = mtcars)
#' p <- predictions(mod)
#' head(p)
#' nrow(p)
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
#' predictions(nom, type = "probs", by = "group") |> summary()
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
#' 
#' @export
predictions <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        vcov = TRUE,
                        conf_level = 0.95,
                        type = NULL,
                        by = NULL,
                        byfun = NULL,
                        wts = NULL,
                        transform_post = NULL,
                        hypothesis = NULL,
                        ...) {


    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
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


    # extracting modeldata repeatedly is slow. this allows marginalmeans to pass modeldata to predictions.
    dots <- list(...)
    if ("modeldata" %in% names(dots)) {
        modeldata <- dots[["modeldata"]]
    } else {
        modeldata <- hush(insight::get_data(model))
    }

    # do not check the model because `insight` supports more models than `marginaleffects`
    # model <- sanitize_model(model)

    # input sanity checks
    checkmate::assert_function(transform_post, null.ok = TRUE)
    sanity_dots(model = model, ...)
    sanity_model_specific(
        model = model,
        newdata = newdata,
        vcov = vcov,
        calling_function = "predictions",
        ...)
    hypothesis <- sanitize_hypothesis(hypothesis, ...)
    conf_level <- sanitize_conf_level(conf_level, ...)
    type <- sanitize_type(model = model, type = type, calling_function = "predictions")
    newdata <- sanitize_newdata(
        model = model,
        newdata = newdata,
        modeldata = modeldata,
        by = by)

    # after sanitize_newdata
    sanity_by(by, newdata)

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

    # weights
    sanity_wts(wts, newdata) # after sanity_newdata
    if (!is.null(wts) && isTRUE(checkmate::check_string(wts))) {
        newdata[["marginaleffects_wts_internal"]] <- newdata[[wts]]
    } else {
        newdata[["marginaleffects_wts_internal"]] <- wts
    }


    # trust newdata$rowid
    if (!"rowid" %in% colnames(newdata)) {
        newdata[["rowid"]] <- seq_len(nrow(newdata))
    }

    # mlogit models sometimes returns an `idx` column that is impossible to `rbind`
    if (inherits(model, "mlogit") && inherits(newdata[["idx"]], "idx")) {
        newdata[["idx"]] <- NULL
    }

    # mlogit uses an internal index that is very hard to track, so we don't
    # support `newdata` and assume no padding the `idx` column is necessary for
    # `get_predict` but it breaks binding, so we can't remove it in
    # sanity_newdata and we can't rbind it with padding

    # pad factors: `get_predicted/model.matrix` break when factor levels are missing
    if (inherits(model, "mlogit")) {
        padding <- data.frame()
    } else {
        padding <- complete_levels(newdata, character_levels)
        if (nrow(padding) > 0) {
            newdata <- rbindlist(list(padding, newdata))
        }
    }

    if (is.null(by)) {
        vcov_tmp <- vcov
    } else {
        vcov_tmp <- FALSE
    }

    J <- NULL

    tmp <- get_predictions(
        model,
        newdata = newdata,
        vcov = vcov_tmp,
        conf_level = conf_level,
        type = type,
        hypothesis = hypothesis,
        by = by,
        byfun = byfun,
        ...)


    # two cases when tmp is a data.frame
    # insight::get_predicted gets us Predicted et al. but now rowid
    # get_predict gets us rowid with the original rows
    if (inherits(tmp, "data.frame")) {
        setnames(tmp,
                 old = c("Predicted", "SE", "CI_low", "CI_high"),
                 new = c("predicted", "std.error", "conf.low", "conf.high"),
                 skip_absent = TRUE)
    } else {
        tmp <- data.frame(newdata$rowid, type, tmp)
        colnames(tmp) <- c("rowid", "type", "predicted")
        if ("rowidcf" %in% colnames(newdata)) {
            tmp[["rowidcf"]] <- newdata[["rowidcf"]]
        }
    }

    # default type column is the `predict()` method
    if (!is.na(type)) {
        tmp$type <- type
    # alternative type is the `insight` name
    } else {
        tmp$type <- names(type)
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
    if (!isFALSE(vcov)) {

        V <- get_vcov(model, vcov = vcov, ...)

        # Delta method
        if (!"std.error" %in% colnames(tmp) && is.null(draws)) {
            if (isTRUE(checkmate::check_matrix(V))) {
                # vcov = FALSE to speed things up
                fun <- function(...) {
                    get_predictions(..., verbose = FALSE, vcov = FALSE)$predicted
                }
                se <- get_se_delta(
                    model,
                    newdata = newdata,
                    vcov = V,
                    type = type,
                    FUN = fun,
                    J = J,
                    eps = 1e-4, # avoid pushing through ...
                    hypothesis = hypothesis,
                    by = by,
                    byfun = byfun,
                    conf_level = conf_level,
                    ...)
                if (is.numeric(se) && length(se) == nrow(tmp)) {
                    tmp[["std.error"]] <- se
                }
            }
        }

        tmp <- get_ci(
            tmp,
            conf_level = conf_level,
            # sometimes insight::get_predicted fails on SE but succeeds on CI (e.g., betareg)
            vcov = vcov,
            overwrite = FALSE,
            draws = draws,
            estimate = "predicted")
    }

    out <- data.table(tmp)

    setDF(out)

    # save weights as attribute and not column
    marginaleffects_wts_internal <- out[["marginaleffects_wts_internal"]]
    out[["marginaleffects_wts_internal"]] <- NULL

    # clean columns
    if (isTRUE(checkmate::check_data_frame(by))) {
        bycols <- setdiff(colnames(by), "by")
    } else {
        bycols <- by
    }

    stubcols <- c( 
        "rowid", "rowidcf", "type", "term", "group", "hypothesis",
        bycols,
        "predicted", "std.error", "statistic", "p.value", "conf.low",
        "conf.high", "marginaleffects_wts",
        sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols, drop = FALSE]

    attr(out, "posterior_draws") <- draws

    # after rename to estimate / after assign draws
    if (is.function(transform_post)) {
        out <- backtransform(out, transform_post = transform_post)
    }

    class(out) <- c("predictions", class(out))
    out <- set_attributes(
        out,
        get_attributes(newdata, include_regex = "^newdata"))
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

    if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
        out$group <- NULL
    }

    return(out)
}


# wrapper used only for standard_error_delta
get_predictions <- function(model,
                            newdata,
                            vcov,
                            conf_level,
                            type,
                            by = NULL,
                            byfun = byfun,
                            hypothesis = NULL,
                            verbose = TRUE,
                            ...) {


    out <- myTryCatch(get_predict(
        model,
        newdata = newdata,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        ...))

    if (inherits(out$value, "data.frame")) {
        out <- out$value
    } else {
        msg <- "Unable to compute predicted values with this model. You can try to supply a different dataset to the `newdata` argument. If this does not work, you can file a report on the Github Issue Tracker: https://github.com/vincentarelbundock/marginaleffects/issues"
        if (!is.null(out$error)) {
            msg <- c(msg, paste("This error was also raised:", out$error$message))
        }
        stop(insight::format_message(msg), call. = FALSE)
    }

    if (!"rowid" %in% colnames(out) && "rowid" %in% colnames(newdata) && nrow(out) == nrow(newdata)) {
        out$rowid <- newdata$rowid
    }

    # extract attributes before setDT
    draws <- attr(out, "posterior_draws")

    setDT(out)

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

    # return data
    # very import to avoid sorting, otherwise bayesian draws won't fit predictions
    # merge only with rowid; not available for hypothesis
    mergein <- setdiff(colnames(newdata), colnames(out))
    if ("rowid" %in% colnames(out) && "rowid" %in% colnames(newdata) && length(mergein) > 0) {
        idx <- c("rowid", mergein)
        tmp <- data.table(newdata)[, ..idx]
        # TODO: this breaks in mclogit. maybe there's a more robust merge
        # solution for weird grouped data. But it seems fine because
        # `predictions()` output does include the original predictors.
        out <- tryCatch(
            merge(out, tmp, by = "rowid", sort = FALSE),
            error = function(e) out)
    }

    # averaging by groups
    out <- get_by(
        out,
        draws = draws,
        newdata = newdata,
        by = by,
        byfun = byfun,
        column = "predicted",
        verbose = verbose,
        ...)

    # after get_by
    draws <- attr(out, "posterior_draws")

    # hypothesis tests using the delta method
    out <- get_hypothesis(out, hypothesis, column = "predicted", by = by)

    return(out)
}

