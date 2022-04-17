#' Adjusted Predictions
#'
#' Calculate adjusted predictions for each row of the dataset. The `datagrid()`
#' function and the `newdata` argument can be used to calculate Average
#' Adjusted Predictions (AAP), Average Predictions at the Mean (APM), or
#' Predictions at User-Specified Values of the regressors (aka Adjusted
#' Predictions at Representative values, APR). See the Details and Examples
#' sections below.
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
#' @param newdata A dataset over which to compute adjusted predictions. `NULL` uses
#'   the original data used to fit the model.
#' @param conf.level The confidence level to use for the confidence interval.
#'   No interval is computed if `conf.int=NULL`.  Must be strictly greater than 0
#'   and less than 1. Defaults to 0.95, which corresponds to a 95 percent
#'   confidence interval.
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
#' pred <- predictions(mod, newdata = datagrid(am = 0, grid.type = "counterfactual")) %>%
#'     summarize(across(c(predicted, std.error), mean))
#'
#' predictions(mod, newdata = datagrid(am = 0:1, grid.type = "counterfactual")) %>% 
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
                        conf.level = 0.95,
                        type = "response",
                        ...) {

    ## do not check this because `insight` supports more models than `marginaleffects`
    # model <- sanity_model(model)
    sanity_dots(model = model, ...)
    sanity_model_specific(model, calling_function = "predictions", ...)

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

    # save all character levels for padding
    # later we call this function again for different purposes
    levels_character <- attr(sanitize_variables(model, newdata = NULL, variables), "levels_character")

    # check before inferring `newdata`
    if (!is.null(variables) && !is.null(newdata)) {
        stop("The `variables` and `newdata` arguments cannot be used simultaneously.")
    } else if (!is.null(variables)) {
        # get new data if it doesn't exist
        newdata <- sanity_newdata(model, newdata)
        variables <- sanitize_variables(model, newdata, variables)
        variables <- unique(unlist(variables))
        args <- list("model" = model)
        for (v in variables) {
            if (is.numeric(newdata[[v]])) {
                args[[v]] <- stats::fivenum(newdata[[v]])
            } else if (is.factor(newdata[[v]]) || is.character(newdata[[v]]) || is.logical(newdata[[v]])) {
                args[[v]] <- unique(newdata[[v]])
            }
        }
        newdata <- do.call("typical", args)
    } else {
        newdata <- sanity_newdata(model, newdata)
        variables <- sanitize_variables(model, newdata, variables)
    }

    # trust newdata$rowid
    if (!"rowid" %in% colnames(newdata)) {
        newdata[["rowid"]] <- seq_len(nrow(newdata))
    }

    # pad factors: `get_predicted/model.matrix` break when factor levels are missing
    padding <- complete_levels(newdata, levels_character)
    newdata <- rbindlist(list(padding, newdata))

    # predictions
    tmp <- get_predict(model,
                       newdata = newdata,
                       vcov = vcov,
                       conf.level = conf.level,
                       type = type,
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
        if ("rowid_counterfactual" %in% colnames(newdata)) {
            tmp[["rowid_counterfactual"]] <- newdata[["rowid_counterfactual"]]
        }
    }
    tmp$type <- type

    if (!"rowid" %in% colnames(tmp) && nrow(tmp) == nrow(newdata)) {
        tmp$rowid <- newdata$rowid
    }

    # try to extract standard errors via the delta method if necessary
    # check conf.low in case it's a bayesian model
    if (!isFALSE(vcov) && !any(c("std.error", "conf.low") %in% colnames(tmp))) {
        V <- get_vcov(model, vcov = vcov)
        if (isTRUE(checkmate::check_matrix(V))) {
            # vcov = FALSE to speed things up
            fun <- function(...) get_predict(vcov = FALSE, ...)[["predicted"]]
            se <- standard_errors_delta(model,
                                        newdata = newdata,
                                        vcov = V,
                                        type = type,
                                        FUN = fun,
                                        ...)
            if (is.numeric(se) && length(se) == nrow(tmp)) {
                tmp[["std.error"]] <- se
                flag <- tryCatch(insight::model_info(model)$is_linear,
                                 error = function(e) FALSE)
                if (isTRUE(flag) && is.numeric(conf.level)) {
                    critical_z <- abs(stats::qnorm((1 - conf.level) / 2))
                    tmp[["conf.low"]] <- tmp[["predicted"]] - critical_z * tmp[["std.error"]]
                    tmp[["conf.high"]] <- tmp[["predicted"]] + critical_z * tmp[["std.error"]]
                }
            }
        }
    }

    out <- data.table(tmp)
    draws <- attr(tmp, "posterior_draws")

    # unpad factors
    out <- out[(nrow(padding) + 1):nrow(out),]
    newdata <- newdata[(nrow(padding) + 1):nrow(newdata), , drop = FALSE]
    if (!is.null(draws)) {
        draws <- draws[(nrow(padding) + 1):nrow(draws), , drop = FALSE]
    }

    # return data
    # very import to avoid sorting, otherwise bayesian draws won't fit predictions
    out <- merge(out, newdata, by = "rowid", sort = FALSE)

    # clean columns
    stubcols <- c("rowid", "type", "term", "group", "predicted", "std.error", "conf.low", "conf.high",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols, drop = FALSE, with = FALSE]

    class(out) <- c("predictions", class(out))
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables

    # modelbased::visualisation_matrix attaches useful info for plotting
    for (a in names(attributes_newdata)) {
        attr(out, paste0("newdata_", a)) <- attributes_newdata[[a]]
    }

    # bayesian: store draws posterior density draws
    attr(out, "posterior_draws") <- draws
    if (!is.null(draws)) {
        flag <- getOption("marginaleffects_credible_interval", default = "eti")
        if (isTRUE(flag == "hdi")) {
            tmp <- apply(draws, 1, get_hdi, credMass = conf.level)
        } else {
            tmp <- apply(draws, 1, get_eti, credMass = conf.level)
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
