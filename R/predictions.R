#' Adjusted Predictions
#'
#' Calculate adjusted predictions for each row of the dataset. The `datagrid()`
#' function and the `newdata` argument can be used to calculate Average
#' Adjusted Predictions (AAP), Average Predictions at the Mean (APM), or
#' Predictions at User-Specified Values of the regressors (aka Adjusted
#' Predictions at Representative values, APR). For more information, see the
#' Details and Examples sections below, and in the vignettes on the
#' `marginaleffects` website: <https://vincentarelbundock.github.io/marginaleffects/>
#' * [Getting Started](https://vincentarelbundock.github.io/marginaleffects/#getting-started)
#' * [Predictions Vignette](https://vincentarelbundock.github.io/marginaleffects/articles/mfx01_predictions.html)
#' * [Supported Models](https://vincentarelbundock.github.io/marginaleffects/articles/mfx06_supported_models.html)
#'
#' An "adjusted prediction" is the outcome predicted by a model for some
#' combination of the regressors' values, such as their observed values, their
#' means, or factor levels (a.k.a. “reference grid”). 

#' When possible, this function uses the delta method to compute the standard
#' error associated with the adjusted predictions.
#'
#' A detailed vignette on adjusted predictions is published on the package
#' website:
#'
#' https://vincentarelbundock.github.io/marginaleffects/

#' Compute model-adjusted predictions (fitted values) for a "grid" of regressor values.
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
#' @export
predictions <- function(model,
                        newdata = NULL,
                        variables = NULL,
                        vcov = TRUE,
                        conf_level = 0.95,
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

    # do not check the model because `insight` supports more models than `marginaleffects`
    # model <- sanitize_model(model)

    # input sanity checks
    sanity_dots(model = model, ...)
    sanity_model_specific(model = model, newdata = newdata, vcov = vcov, calling_function = "predictions", ...)
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
    tmp <- myTryCatch(get_predict(
        model,
        newdata = newdata,
        vcov = vcov,
        conf_level = conf_level,
        type = type,
        ...))

    if (isTRUE(grepl("type.*models", tmp[["error"]]))) {
        stop(tmp$error$message, call. = FALSE)
    } else if (!inherits(tmp[["value"]], "data.frame")) {
        if (!is.null(tmp$warning)) warning(tmp$warning$message, call. = FALSE)
        if (!is.null(tmp$error)) warning(tmp$error$message, call. = FALSE)
        msg <- sprintf("Unable to compute adjusted predictions for model of class `%s`. You can try specifying a different value for the `newdata` argument. If this does not work and you believe that this model class should be supported by `marginaleffects`, please file a feature request on the Github issue tracker: https://github.com/vincentarelbundock/marginaleffects/issues", 
                       class(model)[1])
        stop(msg, call. = FALSE)
    } else if (inherits(tmp[["warning"]], "warning") &&
               isTRUE(grepl("vcov.*supported", tmp)) &&
               !is.null(vcov) &&
               !isFALSE(vcov)) {
        msg <- sprintf("The object passed to the `vcov` argument is of class `%s`, which is not supported for models of class `%s`. Please set `vcov` to `TRUE`, `FALSE`, `NULL`, or supply a variance-covariance `matrix` object.",
                       class(model)[1])
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

    # try to extract standard errors via the delta method if missing
    if (!isFALSE(vcov) &&
        !"std.error" %in% colnames(tmp) &&
        is.null(draws)) {

        V <- get_vcov(model, vcov = vcov)
        if (isTRUE(checkmate::check_matrix(V))) {
            # vcov = FALSE to speed things up
            fun <- function(...) get_predict(vcov = FALSE, ...)[["predicted"]]
            se <- standard_errors_delta(model,
                                        newdata = newdata,
                                        vcov = V,
                                        type = type,
                                        FUN = fun,
                                        eps = 1e-4, # avoid pushing through ...
                                        ...)
            if (is.numeric(se) && length(se) == nrow(tmp)) {
                tmp[["std.error"]] <- se
                flag <- tryCatch(insight::model_info(model)$is_linear,
                                 error = function(e) FALSE)
                if (isTRUE(flag) &&
                    is.numeric(conf_level) &&
                    # sometimes get_predicted fails on SE but succeeds on CI (e.g., betareg)
                    !"conf.low" %in% colnames(tmp)) {
                    critical_z <- abs(stats::qnorm((1 - conf_level) / 2))
                    tmp[["conf.low"]] <- tmp[["predicted"]] - critical_z * tmp[["std.error"]]
                    tmp[["conf.high"]] <- tmp[["predicted"]] + critical_z * tmp[["std.error"]]
                }
            }
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
    out <- merge(out, newdata, by = "rowid", sort = FALSE)

    setDF(out)


    # clean columns
    stubcols <- c("rowid", "type", "term", "group", "predicted", "std.error", "statistic", "p.value", "conf.low", "conf.high",
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

    # modelbased::visualisation_matrix attaches useful info for plotting
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }

    # bayesian: store draws posterior density draws
    attr(out, "posterior_draws") <- draws
    if (!is.null(draws)) {
        flag <- getOption("marginaleffects_credible_interval", default = "eti")
        if (isTRUE(flag == "hdi")) {
            tmp <- apply(draws, 1, get_hdi, credMass = conf_level)
        } else {
            tmp <- apply(draws, 1, get_eti, credMass = conf_level)
        }
        out[["predicted"]] <- apply(draws, 1, stats::median)
        out[["std.error"]] <- NULL
        out[["conf.low"]] <- tmp[1, ]
        out[["conf.high"]] <- tmp[2, ]
        attr(out, "posterior_draws") <- draws
    }


    if ("group" %in% names(out) && all(out$group == "main_marginaleffect")) {
        out$group <- NULL
    }

    return(out)
}
