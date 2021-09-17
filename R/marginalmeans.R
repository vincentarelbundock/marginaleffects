#' Marginal means
#' 
#' Warning: This package is experimental.
#' 
#' @param model Model object
#' @param variables Character vector. Compute Estimated Marginal Means for
#'   combinations of each of these variables. Factor levels are considered at
#'   each of their levels. Numeric variables variables are considered at Tukey's
#'   Five-Number Summaries.
#' @param newdata A dataset over which to compute marginal effects. `NULL` uses
#'   the original data used to fit the model.
#' @param predict_type Type(s) of prediction as string or vector This can
#' differ based on the model type, but will typically be a string such as:
#' "response", "link", "probs", or "zero".
#' @param ... Additional arguments are pushed forward to `predict()`.
#' @export
#' @details
marginalmeans <- function(model, 
                          newdata = NULL, 
                          variables = NULL, 
                          predict_type = "response",
                          ...) {

    # sanity checks and pre-processing
    predict_type <- sanity_predict_type(model, predict_type)
    # return_data <- sanity_return_data(return_data)

    ## do not check this because `insight` supports more models than `marginaleffects`
    # model <- sanity_model(model)

    # order of the first few paragraphs is important
    # if `newdata` is a call to `typical()` or `counterfactual()`, insert `model`
    scall <- substitute(newdata)
    if (is.call(scall) && as.character(scall)[1] %in% c("typical", "counterfactual")) {
        lcall <- as.list(scall)
        if (!any(c("model", "data") %in% names(lcall))) {
            lcall <- c(lcall, list("model" = model))
            newdata <- eval.parent(as.call(lcall))
        }
    }

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
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- 1:nrow(newdata)
    }

    # predictions
    out_list <- list()
    for (predt in predict_type) {
        tmp <- insight::get_predicted(model, 
                                      newdata = newdata,
                                      type = predt)
        tmp <- as.data.frame(tmp)
        tmp <- insight::standardize_names(tmp, style = "broom")
        tmp$type <- predt
        tmp$rowid <- 1:nrow(tmp)
        out_list[[predt]] <- tmp
    }
    out <- do.call("rbind", out_list)

    # return data
    out <- merge(out, newdata, by = "rowid")

    # rowid does not make sense here because the grid is made up
    out$rowid <- NULL

    # clean columns
    stubcols <- c("rowid", "type", "group", "term", "predicted", "std.error",
                  "conf.low", "conf.high", 
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols]
    if ("group" %in% colnames(out) && all(out$group == "main")) {
        out$group <- NULL
    }
    row.names(out) <- NULL

    # attach model info
    if (isTRUE(check_dependency("modelsummary"))) {
        gl <- suppressWarnings(try(modelsummary::get_gof(model), silent = TRUE))
        if (inherits(gl, "data.frame")) {
            attr(out, "glance") <- data.frame(gl)
        } else {
            attr(out, "glance") <- NULL
        }
    } else {
        attr(out, "glance") <- NULL
    }
    class(out) <- c("marginalmeans", class(out))
    attr(out, "predict_type") <- predict_type
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables

    return(out)
}


#' `meffects()` is a shortcut to `marginaleffects()`
#'
#' @inherit marginaleffects
#' @keywords internal
#' @export
meffects <- marginaleffects
