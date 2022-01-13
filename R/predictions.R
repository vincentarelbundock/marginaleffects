#' Adjusted Predictions
#'
#' This function calculates adjusted predictions for each row of the dataset.
#' The `datagrid()` function and the `newdata` argument can be used to
#' calculate Average Adjusted Predictions (AAP), Average Prediction at the Mean
#' (APM), or Predictions at User-Specified Values of the regressors. See below
#' for details and examples.
#'
#' An "ajusted prediction" is the outcome predicted by a model for some
#' combination of the regressors’ values, such as their means or factor levels
#' (a.k.a. “reference grid”). When possible, this function uses the delta
#' method to compute the standard error associated with the adjusted
#' predictions.
#'
#' A detailed vignette on adjusted predictions and a list of supported models
#' are published on the package website:
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
#' No interval is computed if `conf.int=NULL`.  Must be strictly greater than 0
#' and less than 1. Defaults to 0.95, which corresponds to a 95 percent
#' confidence interval.
#' @param ... Additional arguments are pushed forward to `predict()`.
#' @return A `data.frame` with one row per observation and several columns:
#' * `rowid`: row number of the `newdata` data frame
#' * `type`: prediction type, as defined by the `type` argument
#' * `group`: (optional) value of the grouped outcome (e.g., categorical outcome models)
#' * `predicted`: predicted outcome
#' * `std.error`: standard errors computed by the `insight::get_predicted` function or, if unavailable, via `marginaleffects` delta method functionality.
#' * `conf.low`: lower bound of the confidence or highest density interval (for bayesian models)
#' * `conf.high`: upper bound of the confidence or highest density interval (for bayesian models)
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
                        variables = NULL,
                        newdata = NULL,
                        conf.level = 0.95,
                        type = "response",
                        ...) {

    ## do not check this because `insight` supports more models than `marginaleffects`
    # model <- sanity_model(model)

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical` or `counterfactual`, insert `model`
    scall <- substitute(newdata)
    if (is.call(scall) && as.character(scall)[1] %in% c("datagrid", "typical", "counterfactual")) {
        lcall <- as.list(scall)
        if (!any(c("model", "newdata") %in% names(lcall))) {
            lcall <- c(lcall, list("model" = model))
            newdata <- eval.parent(as.call(lcall))
        }
    }

    # save all character levels for padding
    # later we call call this function again for different purposes
    levels_character <- attr(sanity_variables(model, newdata, variables), "levels_character")

    # check before inferring `newdata`
    if (!is.null(variables) && !is.null(newdata)) {
        stop("The `variables` and `newdata` arguments cannot be used simultaneously.")
    } else if (!is.null(variables)) {
        # get new data if it doesn't exist
        newdata <- sanity_newdata(model, newdata)
        variables <- sanity_variables(model, newdata, variables)
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
        variables <- sanity_variables(model, newdata, variables)
    }

    # for merging later
    newdata$rowid_internal <- 1:nrow(newdata)

    # pad factors: `get_predicted/model.matrix` break when factor levels are missing
    padding <- complete_levels(newdata, levels_character)
    newdata <- rbind(padding, newdata)

    # predictions
    out_list <- list()
    draws_list <- list()
    for (predt in type) {
        # extract
        tmp <- get_predict(model,
                           newdata = newdata,
                           type = predt,
                           conf.level = conf.level,
                           ...)

        if (inherits(tmp, "data.frame")) {
            colnames(tmp)[colnames(tmp) == "Predicted"] <- "predicted"
            colnames(tmp)[colnames(tmp) == "SE"] <- "std.error"
            colnames(tmp)[colnames(tmp) == "CI_low"] <- "conf.low"
            colnames(tmp)[colnames(tmp) == "CI_high"] <- "conf.high"
            tmp$rowid_internal <- newdata$rowid_internal
            tmp$type <- predt
        } else {
            tmp <- data.frame(newdata$rowid_internal, predt, tmp)
            colnames(tmp) <- c("rowid_internal", "type", "predicted")
        }

        # try to extract standard errors via the delta method if necessary
        if (is.numeric(conf.level) && 
            !any(c("std.error", "conf.low") %in% colnames(tmp))) {

            vcov <- try(get_vcov(model), silent = TRUE)
            if (!inherits(vcov, "try-error") && is.matrix(vcov)) {
                fun <- function(...) get_predict(...)[["predicted"]]
                se <- standard_errors_delta(model,
                                            newdata = newdata,
                                            vcov = get_vcov(model),
                                            type = predt,
                                            FUN = fun,
                                            ...)
                if (is.numeric(se) && length(se) == nrow(tmp)) {
                    tmp[["std.error"]] <- se
                }
            }
        }

        out_list[[predt]] <- tmp
        draws <- attr(tmp, "posterior_draws")
        draws_list[[predt]] <- draws
    }

    out <- bind_rows(out_list)
    draws <- do.call("rbind", draws_list) # poorman::bind_rows does not work on matrices

    # unpad factors
    idx <- out$rowid_internal > 0
    out <- out[idx, , drop = FALSE]
    draws <- draws[idx, , drop = FALSE]

    # return data
    # base::merge() mixes row order
    out <- left_join(out, newdata, by = "rowid_internal")

    # rowid does not make sense here because the grid is made up
    # Wrong! rowid does make sense when we use `counterfactual()` in `newdata`
    out$rowid_internal <- NULL

    # clean columns
    stubcols <- c("rowid", "type", "term", "group", "predicted", "std.error", "conf.low", "conf.high",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols, drop = FALSE]
    row.names(out) <- NULL

    # we want consistent output, regardless of whether `data.table` is installed/used or not
    out <- as.data.frame(out)

    class(out) <- c("predictions", class(out))
    attr(out, "model") <- model
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables

    # bayesian: store draws posterior density draws
    attr(out, "posterior_draws") <- draws
    if (!is.null(draws)) {
        tmp <- apply(draws, 1, get_hdi, credMass = conf.level)
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
