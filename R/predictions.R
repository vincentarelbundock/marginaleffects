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
#' predictions.
#'
#' @inheritParams marginaleffects
#' @param model Model object
#' @param variables Named list of variables with values to create a
#' counterfactual grid of predictions. The entire dataset replicated
#' for each unique combination of the variables in this list. See the Examples
#' section below. Warning: This can use a lot of memory if there are many
#' variables and values, and when the dataset is large.
#' @param newdata `NULL`, data frame, string, or `datagrid()` call. Determines the grid of predictors on which we make predictions.
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
#' @param newdata A data frame over which to compute quantities of interest.
#'   + `NULL`: adjusted predictions for each observed value in the original dataset.
#'   - "mean": Marginal Effects at the Mean. Marginal effects when each predictor is held at its mean or mode.
#'   - "median": Marginal Effects at the Median. Marginal effects when each predictor is held at its median or mode.
#'   - "marginalmeans": Marginal Effects at Marginal Means. See Details section below.
#'   - "tukey": Marginal Effects at Tukey's 5 numbers.
#'   - "grid": Marginal Effects on a grid of representative numbers (Tukey's 5 numbers and unique values of categorical predictors).

#'   + The [datagrid()] function can be used to specify a custom grid of regressors. For example:
#'       - `newdata = datagrid()`: contrast at the mean
#'       - `newdata = datagrid(cyl = c(4, 6))`: `cyl` variable equal to 4 and 6 and other regressors fixed at their means or modes.
#'       - See the Examples section and the [datagrid()] documentation for more.
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
#' pred <- predictions(mod, newdata = datagrid(am = 0, grid_type = "counterfactual")) %>%
#'     summarize(across(c(predicted, std.error), mean))
#'
#' predictions(mod, newdata = datagrid(am = 0:1, grid_type = "counterfactual")) %>% 
#'     group_by(am) %>%
#'     summarize(across(c(predicted, std.error), mean))
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
#' @export
predictions <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        vcov = TRUE,
                        conf_level = 0.95,
                        type = "response",
                        by = NULL,
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

    # do not check the model because `insight` supports more models than `marginaleffects`
    # model <- sanitize_model(model)

    # input sanity checks
    checkmate::assert_function(transform_post, null.ok = TRUE)
    sanity_dots(model = model, ...)
    sanity_model_specific(
        model = model,
        newdata = newdata,
        vcov = vcov,
        calling_function = "predictions", ...)
    hypothesis <- sanitize_hypothesis(hypothesis, ...)
    conf_level <- sanitize_conf_level(conf_level, ...)
    type <- sanitize_type(model = model, type = type, calling_function = "predictions")
    newdata <- sanitize_newdata(model = model, newdata = newdata)

    # `variables` si character vector: Tukey's 5 or uniques
    checkmate::assert_list(variables, names = "unique", null.ok = TRUE)

    # analogous to comparisons(variables=list(...))
    if (!is.null(variables)) {
        args <- list(
            "model" = model,
            "newdata" = newdata,
            "grid_type" = "counterfactual")
        tmp <- sanitize_variables(
            variables = variables,
            model = model,
            newdata = newdata)$conditional
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

    tmp <- myTryCatch(get_predictions(
        model,
        newdata = newdata,
        vcov = vcov_tmp,
        conf_level = conf_level,
        type = type,
        hypothesis = hypothesis,
        by = by,
        ...))

    if (isTRUE(grepl("type.*models", tmp[["error"]]))) {
        stop(tmp$error$message, call. = FALSE)

    } else if (!inherits(tmp[["value"]], "data.frame")) {
        if (isTRUE(grepl("row indices", tmp$error$message))) stop(tmp$error$message, call. = FALSE)
        if (!is.null(tmp$warning)) warning(tmp$warning$message, call. = FALSE)
        if (!is.null(tmp$error)) warning(tmp$error$message, call. = FALSE)
        msg <- format_msg(
            "Unable to compute adjusted predictions for model of class `%s`. You can try to
            specify a different value for the `newdata` argument. If this does not work and
            you believe that this model class should be supported by `marginaleffects`,
            please file a feature request on the Github issue tracker:

            https://github.com/vincentarelbundock/marginaleffects/issues")
        msg <- sprintf(msg, class(model)[1])
        stop(msg, call. = FALSE)

    } else if (inherits(tmp[["warning"]], "warning") &&
               isTRUE(grepl("vcov.*supported", tmp)) &&
               !is.null(vcov) &&
               !isFALSE(vcov)) {
        msg <- format_msg(
            "The object passed to the `vcov` argument is of class `%s`, which is not
            supported for models of class `%s`. Please set `vcov` to `TRUE`, `FALSE`,
            `NULL`, or supply a variance-covariance `matrix` object.")
        msg <- sprintf(msg, class(model)[1])
        stop(msg, call. = FALSE)

    } else if (inherits(tmp[["warning"]], "warning")) {
        msg <- tmp$warning$message
        warning(msg, call. = FALSE)
        tmp <- tmp[["value"]]

    } else {
        tmp <- tmp[["value"]]
    }


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
            insight::get_df(model, data = newdata, type = vcov),
            error = function(e) NULL)
        if (isTRUE(length(df) == nrow(tmp))) {
            tmp$df <- df
        }
    }


    # bayesian posterior draws
    draws <- attr(tmp, "posterior_draws")

    # bayesian: unpad draws (done in get_predictions for frequentist)
    if (!is.null(draws) && "rowid" %in% colnames(newdata)) {
        draws <- draws[newdata$rowid > 0, , drop = FALSE]
    }

    if (!is.null(transform_post)) {
        draws <- transform_post(draws)
    }

    V <- NULL
    if (!isFALSE(vcov)) {

        V <- get_vcov(model, vcov = vcov)

        # Delta method
        if (!"std.error" %in% colnames(tmp) && is.null(draws)) {
            if (isTRUE(checkmate::check_matrix(V))) {
                # vcov = FALSE to speed things up
                fun <- function(...) get_predictions(..., vcov = FALSE)$predicted
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
                    conf_level = conf_level,
                    ...)
                if (is.numeric(se) && length(se) == nrow(tmp)) {
                    tmp[["std.error"]] <- se
                }
            }
        }

        # Manual confidence intervals only in linear or Bayesian models
        # others rely on `insight::get_predicted()`
        linpred <- tryCatch(
            insight::model_info(model)$is_linear || type == "link",
            error = function(e) FALSE)
        if (!is.null(draws) || isTRUE(linpred)) {
            tmp <- get_ci(
                tmp,
                conf_level = conf_level,
                # sometimes insight::get_predicted fails on SE but succeeds on CI (e.g., betareg)
                vcov = vcov,
                overwrite = FALSE,
                draws = draws,
                estimate = "predicted")
        }
    }

    out <- data.table(tmp)


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


    setDF(out)

    # save as attribute and not column
    marginaleffects_wts_internal <- out[["marginaleffects_wts_internal"]]
    out[["marginaleffects_wts_internal"]] <- NULL

    # transform already applied to bayesian draws before computing confidence interval
    if (is.null(draws) && !is.null(transform_post)) {
        out <- backtransform(out, transform_post = transform_post)
    }

    # clean columns
    stubcols <- c( 
        "rowid", "rowidcf", "type", "term", "group", "hypothesis",
        by,
        "predicted", "std.error", "statistic", "p.value", "conf.low",
        "conf.high", "marginaleffects_wts",
        sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols, drop = FALSE]

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
    attr(out, "posterior_draws") <- draws
    attr(out, "newdata") <- newdata
    attr(out, "weights") <- marginaleffects_wts_internal
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
                            hypothesis = NULL,
                            ...) {


    out <- get_predict(
        model,
        newdata = newdata,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        ...)
    setDT(out)

    # unpad factors before averaging
    if ("rowid" %in% colnames(out)) {
        out <- out[rowid > 0, drop = FALSE]
    }

    # averaging by groups
    if (!is.null(by)) {
        tmp <- intersect(
            c("rowid", "marginaleffects_wts_internal", by),
            colnames(newdata))
        tmp <- data.frame(newdata)[, tmp]
        out <- merge(out, tmp, by = "rowid")
        if ("marginaleffects_wts_internal" %in% colnames(newdata)) {
            out <- out[,
            .(predicted = stats::weighted.mean(
                predicted,
                marginaleffects_wts_internal,
                na.rm = TRUE)),
            by = by]
        } else {
            out <- out[,
            .(predicted = mean(predicted)),
            by = by]
        }
    }

    if (!is.null(hypothesis)) {
        out <- get_hypothesis(out, hypothesis, column = "predicted")
    }

    return(out)
}
