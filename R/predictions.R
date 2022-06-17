#' Adjusted Predictions
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
#' @param variables Character vector. Compute Adjusted Predictions for
#'   combinations of each of these variables. Factor levels are considered at
#'   each of their levels. Numeric variables variables are considered at Tukey's
#'   Five-Number Summaries. `NULL` uses the original data used to fit the model.
#' @param newdata A data frame over which to compute quantities of interest.
#'   + `NULL`: adjusted predictions for each observed value in the original dataset.
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

    # do not check the model because `insight` supports more models than `marginaleffects`
    # model <- sanitize_model(model)

    # input sanity checks
    checkmate::assert_function(transform_post, null.ok = TRUE)
    sanity_dots(model = model, ...)
    sanity_model_specific(model = model, newdata = newdata, vcov = vcov, calling_function = "predictions", ...)
    hypothesis <- sanitize_hypothesis(hypothesis, ...)
    conf_level <- sanitize_conf_level(conf_level, ...)
    levels_character <- attr(variables, "levels_character")

    # modelbased::visualisation_matrix attaches useful info for plotting
    attributes_newdata <- attributes(newdata)
    idx <- c("class", "row.names", "names", "data", "reference")
    idx <- !names(attributes_newdata) %in% idx
    attributes_newdata <- attributes_newdata[idx]

    # after modelbased attribute extraction
    newdata <- sanity_newdata(model, newdata)

    # check before inferring `newdata`
    if (!is.null(variables)) {
        variables <- sanitize_variables(model, newdata, variables)
        # get new data if it doesn't exist
        variables <- unique(unlist(variables))
        args <- list("newdata" = newdata, "model" = model)
        for (v in variables) {
            vcl <- find_variable_class(v, newdata = newdata, model = model)
            if (isTRUE(vcl == "numeric")) {
                args[[v]] <- stats::fivenum(newdata[[v]])
            } else {
                args[[v]] <- unique(newdata[[v]])
            }
        }
        newdata <- do.call("datagrid", args)
        newdata[["rowid"]] <- NULL # the original rowids are no longer valid after averaging et al.
    } else {
        variables <- sanitize_variables(model, newdata, variables)
    }

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
        padding <- complete_levels(newdata, levels_character)
        newdata <- rbindlist(list(padding, newdata))
    }

    # predictions
    # the default get_predict() method tries to get confidence intervals using
    # `insight::get_predicted`. That function does not preserve J, which we
    # need for average adjusted predictions. So we take fast predictions and
    # handle SE internally for known models.
    flag <- isTRUE(class(model)[1] == "lm") ||
            (isTRUE(class(model)[1] == "glm") && isTRUE(type == "link"))

    if (isTRUE(flag)) {
        vcov_tmp <- FALSE
        # get_modelmatrix() sometimes breaks when there is no outcome in `data`
        resp <- insight::find_response(model)
        if (!resp %in% colnames(newdata)) {
            newdata[[resp]] <- 0
        }
        J <- insight::get_modelmatrix(model, data = newdata)
    } else {
        vcov_tmp <- vcov
        J <- NULL
    }
                        
    tmp <- myTryCatch(get_predictions(
        model,
        newdata = newdata,
        vcov = vcov_tmp,
        conf_level = conf_level,
        type = type,
        hypothesis = hypothesis,
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
        if ("rowid_counterfactual" %in% colnames(newdata)) {
            tmp[["rowid_counterfactual"]] <- newdata[["rowid_counterfactual"]]
        }
    }
    tmp$type <- type

    if (!"rowid" %in% colnames(tmp) && nrow(tmp) == nrow(newdata)) {
        tmp$rowid <- newdata$rowid
    }

    # bayesian posterior draws
    draws <- attr(tmp, "posterior_draws")
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
                fun <- function(...) get_predictions(vcov = FALSE, ...)$predicted
                se <- get_se_delta(
                    model,
                    newdata = newdata,
                    vcov = V,
                    type = type,
                    FUN = fun,
                    J = J,
                    eps = 1e-4, # avoid pushing through ...
                    hypothesis = hypothesis,
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
                overwrite = FALSE,
                draws = draws,
                estimate = "predicted")
        }

    }

    out <- data.table(tmp)

    # unpad factors
    out <- out[(nrow(padding) + 1):nrow(out),]
    newdata <- newdata[(nrow(padding) + 1):nrow(newdata), , drop = FALSE]
    if (!is.null(draws)) {
        draws <- draws[(nrow(padding) + 1):nrow(draws), , drop = FALSE]
    }

    # return data
    # very import to avoid sorting, otherwise bayesian draws won't fit predictions
    # merge only with rowid; not available for hypothesis
    if ("rowid" %in% colnames(out)) {
        out <- merge(out, newdata, by = "rowid", sort = FALSE)
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
        "rowid", "type", "term", "group", "hypothesis", "predicted", "std.error",
        "statistic", "p.value", "conf.low", "conf.high", "marginaleffects_wts",
        sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols, drop = FALSE]

    class(out) <- c("predictions", class(out))
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables
    attr(out, "vcov.type") <- get_vcov_label(vcov)
    attr(out, "jacobian") <- J
    attr(out, "vcov") <- V
    attr(out, "posterior_draws") <- draws
    attr(out, "newdata") <- newdata
    attr(out, "weights") <- marginaleffects_wts_internal

    # modelbased::visualisation_matrix attaches useful info for plotting
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }

    if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
        out$group <- NULL
    }

    return(out)
}

 
# wrapper used only for standard_error_delta
get_predictions <- function(..., hypothesis = NULL) {
    out <- get_predict(...)
    if (!is.null(hypothesis)) {
        out <- get_hypothesis(out, hypothesis, column = "predicted")
    }
    return(out)
}
