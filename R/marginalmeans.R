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
#' @param numDeriv_method One of "simple", "Richardson", or "complex",
#'   indicating the method to use for the approximation. See
#'   [numDeriv::grad()] for details.
#' @param return_data boolean If `TRUE`, the original data used to fit the
#'   model is attached to the output. `FALSE` will objects which take up less
#'   space in memory.
#' @param ... Additional arguments are pushed forward to `predict()`.
#' @export
#' @details
marginalmeans <- function(model, 
                          newdata = NULL, 
                          variables = NULL, 
                          numDeriv_method = "simple",
                          predict_type = "response",
                          return_data = TRUE,
                          ...) {


    
  
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

    ########################################
    #  TODO: Remove this when implemented  #
    ########################################

    if (!is.null(newdata)) {
        stop("The `newdata` argument of `marginalmeans` is not yet implemented.")
    }

    if (is.null(newdata) && is.null(variables)) {
        stop("Please supply a value for one of the `variables` or `newdata` arguments in the `marginalmeans` function.")
    }

    if (!is.null(variables) && !is.null(newdata)) {
        stop("The `variables` and `newdata` arguments cannot be used simultaneously.")
    }

    # sanity checks and pre-processing
    model <- sanity_model(model)
    predict_type <- sanity_predict_type(model, predict_type)
    return_data <- sanity_return_data(return_data)
    
    # variables argument
    if (!is.null(newdata)) {
        newdata <- sanity_newdata(model, newdata)
        variables <- sanity_variables(model, newdata, variables)
    } else {
        newdata <- sanity_newdata(model, newdata)
        variables <- sanity_variables(model, newdata, variables)
        args <- list("model" = model)
        for (v in variables$contrast) {
            args[[v]] <- unique(newdata[[v]])
        }
        for (v in variables$dydx) {
            args[[v]] <- stats::fivenum(newdata[[v]])
        }
        newdata <- do.call("typical", args)
    }

    newdata$rowid <- 1:nrow(newdata)

    # predictions
    for (predt in predict_type) {
        out_list <- list()
        tmp <- insight::get_predicted(model, 
                                      newdata = newdata,
                                      predict = predt)
        tmp <- as.data.frame(tmp)
        tmp <- insight::standardize_names(tmp, style = "broom")
        tmp$type <- predt
        tmp$rowid <- 1:nrow(tmp)
        out_list[[predt]] <- tmp
    }
    out <- dplyr::bind_rows(out_list)

    # return data
    if (isTRUE(return_data)) {
        out <- merge(out, newdata, by = "rowid")
    }

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
    attr(out, "numDeriv_method") <- numDeriv_method
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
