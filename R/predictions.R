#' Adjusted Predictions
#'
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
#' @return A `data.frame` with a `predicted` column with predictions.
#' @export
predictions <- function(model,
                        variables = NULL,
                        newdata = NULL,
                        conf.level = 0.95,
                        type = "response",
                        ...) {

    # sanity checks and pre-processing
    type <- sanity_type(model, type)

    ## do not check this because `insight` supports more models than `marginaleffects`
    # model <- sanity_model(model)

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical()` or `counterfactual()`, insert `model`
    scall <- substitute(newdata)
    if (is.call(scall) && as.character(scall)[1] %in% c("typical", "counterfactual")) {
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
    } else if (is.null(newdata) && is.null(variables)) {
        newdata <- typical(model = model)
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
    } else if (!is.null(newdata)) {
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
    for (predt in type) {
        # extract
        tmp <- try(insight::get_predicted(model,
                                          data = newdata,
                                          predict = NULL,
                                          type = predt,
                                          ci = conf.level),
                   silent = TRUE)
        if (inherits(tmp, "try-error")) {
            tmp <- get_predict(model, newdata = newdata, type = predt)
        }

        # process
        if (inherits(tmp, "get_predicted")) {
            tmp <- as.data.frame(tmp)
            tmp <- insight::standardize_names(tmp, style = "broom")
            tmp$type <- predt
            tmp$rowid_internal <- newdata$rowid_internal
        } else {
            tmp <- data.frame(newdata$rowid_internal, predt, tmp)
            colnames(tmp) <- c("rowid_internal", "type", "predicted")
        }
        out_list[[predt]] <- tmp
    }
    out <- do.call("rbind", out_list)

    # unpad factors
    out <- out[out$rowid_internal > 0, , drop = FALSE]

    # return data
    out <- merge(out, newdata, all.x = TRUE, sort = FALSE)

    # rowid does not make sense here because the grid is made up
    # Wrong! rowid does make sense when we use `counterfactual()` in `newdata`
    out$rowid_internal <- NULL

    # clean columns
    stubcols <- c("rowid", "type", "term", "predicted", "std.error", "conf.low", "conf.high",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols]
    row.names(out) <- NULL

    # attach model info
    if (isTRUE(check_dependency("modelsummary"))) {
        gl <- suppressMessages(suppressWarnings(try(modelsummary::get_gof(model), silent = TRUE)))
        if (inherits(gl, "data.frame")) {
            attr(out, "glance") <- data.frame(gl)
        } else {
            attr(out, "glance") <- NULL
        }
    } else {
        attr(out, "glance") <- NULL
    }
    class(out) <- c("predictions", class(out))
    attr(out, "type") <- type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables

    return(out)
}
