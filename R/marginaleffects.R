#' Marginal effects using numerical derivatives
#' 
#' A "marginal effect" is the partial derivative of the regression equation
#' with respect to a variable in the model. This function uses automatic
#' differentiation to compute marginal effects for a vast array of models,
#' including non-linear models with transformations (e.g., polynomials). The
#' list of supported models and of models whose numerical results have been
#' validated against external software (`Stata` or `margins`) is held in the
#' `supported_models` dataset which accompanies this package.
#' 
#' @param model Model object
#' @param variables Variables to consider (character vector). `NULL`
#'   calculates marginal effects for all terms in the model object.
#' @param vcov Matrix or boolean
#'   + FALSE: does not compute unit-level standard errors.
#'   + TRUE: computes unit-level standard errors using the default `vcov(model)` variance-covariance matrix.
#'   + Named square matrix: computes standard errors with a user-supplied variance-covariance matrix. This matrix must be square and have dimensions equal to the number of coefficients in `get_coef(model)`.
#' @param newdata A dataset over which to compute marginal effects. `NULL` uses
#'   the original data used to fit the model.
#' @param type Type(s) of prediction as string or vector This can
#' differ based on the model type, but will typically be a string such as:
#' "response", "link", "probs", or "zero".
#' @param ... Additional arguments are pushed forward to `predict()`.
#' @return A data.frame of marginal effect estimates with one row per observation per marginal effect.
#' @export
#' @examples
#'
#' mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
#' mfx <- marginaleffects(mod)
#' summary(mfx)
#' tidy(mfx)
#' head(mfx)
#' plot(mfx)
#'
#' # typical marginal effects
#' marginaleffects(mod,
#'                 newdata = typical(hp = c(100, 110)))
#'
#' # counterfactual average marginal effects
#' marginaleffects(mod,
#'                 newdata = counterfactual(hp = c(100, 110)))
#'
#' # heteroskedasticity robust standard errors
#' marginaleffects(mod, vcov = sandwich::vcovHC(mod))
#'
marginaleffects <- function(model,
                            newdata = NULL,
                            variables = NULL,
                            vcov = TRUE,
                            type = "response",
                            ...) {


    # should I expose this to users? For now, keep the UI simple
    # @param numDeriv_method One of "simple", "Richardson", or "complex",
    #   indicating the method to use for the approximation. See
    #   [numDeriv::grad()] for details.
    # @param return_data boolean If `TRUE`, the original data used to fit the
    #   model is attached to the output. `FALSE` will objects which take up less
    #   space in memory.
    numDeriv_method <- "simple"
    return_data <- TRUE

    # if `newdata` is a call to `typical()` or `counterfactual()`, insert `model`
    scall <- substitute(newdata)
    if (is.call(scall) && as.character(scall)[1] %in% c("typical", "counterfactual")) {
        lcall <- as.list(scall)
        if (!any(c("model", "data") %in% names(lcall))) {
            lcall <- c(lcall, list("model" = model))
            newdata <- eval.parent(as.call(lcall))
        }
    }

    # sanity checks and pre-processing
    model <- sanity_model(model = model, 
                          newdata = newdata,
                          variables = variables,
                          vcov = vcov,
                          type = type,
                          return_data = return_data,
                          ...)
    newdata <- sanity_newdata(model, newdata)
    variables <- sanity_variables(model, newdata, variables)
    vcov <- sanity_vcov(model, vcov)
    type <- sanity_type(model, type)
    return_data <- sanity_return_data(return_data)

    group_names <- get_group_names(model)

    # rowid is required for later merge
    if (!"rowid" %in% colnames(newdata)) {
        newdata$rowid <- 1:nrow(newdata)
    }

    # variables is a list, get_dydx_and_se() needs a vector
    variables_vec <- unlist(variables[names(variables) %in% c("conditional")])

    pred_list <- list()
    # add predictions to newdata
    for (gn in group_names) {
        for (predt in type) {
            tmp <- newdata
            tmp$type <- predt
            tmp$predicted <- get_predict(model = model,
                                         newdata = newdata,
                                         type = predt,
                                         group_name = gn,
                                         ...)
            colnames(tmp) <- gsub("_main_marginaleffect", "", colnames(tmp))
            pred_list[[predt]] <- tmp
        }
    }
    pred <- do.call("rbind", pred_list)

    # compute marginal effects and standard errors
    out_list <- list()
    se_list <- list()
    for (gn in group_names) {
        out_list[[gn]] <- list()
        se_list[[gn]] <- list()
        for (predt in type) {
            out_list[[gn]][[predt]] <- get_dydx_and_se(
                model = model,
                fitfram = newdata,
                variables = variables_vec,
                vcov = vcov,
                group_name = gn,
                type = predt,
                numDeriv_method = numDeriv_method,
                ...)
            se_list[[gn]][[predt]] <- attr(out_list[[gn]][[predt]], "se_at_mean_gradient")
            se_list[[gn]][[predt]]$type <- predt
            out_list[[gn]][[predt]]$type <- predt
            se_list[[gn]][[predt]]$group <- gn
            out_list[[gn]][[predt]]$group <- gn
        }
        se_list[[gn]] <- do.call("rbind", se_list[[gn]])
        out_list[[gn]] <- do.call("rbind", out_list[[gn]])
    }
    out <- do.call("rbind", out_list)
    attributes_backup <- attributes(out)
    se <- do.call("rbind", se_list)
    row.names(se) <- NULL

    # merge newdata if requested and restore attributes
    if (return_data) {
        out <- merge(out, pred, by = c("rowid", "type"))
    }

    # clean columns
    stubcols <- c("rowid", "type", "group", "term", "dydx", "std.error",
                  sort(grep("^predicted", colnames(newdata), value = TRUE)))
    cols <- intersect(stubcols, colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols]
    if ("group" %in% colnames(out) && all(out$group == "main_marginaleffect")) {
        out$group <- NULL
    }

    # sort rows
    out <- out[order(out$type, out$term, out$rowid), ]
    row.names(out) <- NULL

    # restore useful attributes lost in "clean columns" bloc
    for (n in names(attributes_backup)) {
        if (!n %in% names(attributes(out))) {
            attr(out, n) <- attributes_backup[[n]]
        }
    }

    # attributes
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
    class(out) <- c("marginaleffects", class(out))
    attr(out, "type") <- type
    attr(out, "numDeriv_method") <- numDeriv_method
    attr(out, "model_type") <- class(model)[1]
    attr(out, "variables") <- variables

    if ("group" %in% colnames(se) && all(se$group == "main_marginaleffect")) {
        se$group <- NULL
    }
    attr(out, "se_at_mean_gradient") <- se

    return(out)
}


#' `meffects()` is a shortcut to `marginaleffects()`
#'
#' @inherit marginaleffects
#' @keywords internal
#' @export
meffects <- marginaleffects
