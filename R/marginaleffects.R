#' Marginal effects using numerical derivatives
#' 
#' Warning: This package is experimental.
#' 
#' A "marginal effect" is the partial derivative of the regression equation
#' with respect to a variable in the model. This package uses automatic
#' differentiation tu compute marginal effects for a vast array of models,
#' including non-linear models with transformations (e.g., polynomials).
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
#' @param prediction_type Type(s) of prediction as string or vector This can
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
#' Most -- but not all -- of the models below have been checked against
#' alternative software packages to validate results. Visit the
#' `marginaleffects` website to learn more about the testing procedure and
#' coverage.
#'
#' Supported models:
#' 
#' + stats::lm
#' + stats::glm
#' + stats::loess (no variance)
#' + AER::ivreg
#' + AER::tobit
#' + betareg::betareg
#' + fixest::feols
#' + fixest::feglm
#' + lme4::lmer
#' + lme4::glmer
#' + MASS::polr (no variance)
#' + ordinal::clm (no variance)
#' + pscl::hurdle
#' + survey::svyglm
#'
#' @examples
#' 
#' mod <- glm(am ~ hp * wt, data = mtcars, family = binomial)
#' mfx <- marginaleffects(mod)
#' summary(mfx)
#' tidy(mfx)
#' head(mfx)
#' \dontrun{
#' plot(mfx)
#' }
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
                            numDeriv_method = "simple",
                            prediction_type = "response",
                            return_data = TRUE,
                            ...) {

  
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
    model <- sanity_model(model)
    newdata <- sanity_newdata(model, newdata)
    variables <- sanity_variables(model, newdata, variables)
    vcov <- sanity_vcov(model, vcov)
    group_names <- sanity_group_names(model)
    prediction_type <- sanity_prediction_type(model, prediction_type)
    return_data <- sanity_return_data(return_data)

    # add predictions to newdata
    for (predt in prediction_type) {
        lab <- paste0("predicted_", predt)
        newdata[[lab]] <- get_predict(model = model,
                                      newdata = newdata,
                                      prediction_type = predt,
                                      ...)
    }

    # dydx: numeric variables w/ autodiff
    dydx <- list()
    for (predt in prediction_type) {
        for (gn in group_names) {
            for (v in variables$dydx) {
                tmp <- get_dydx_and_se(model = model, 
                                       fitfram = newdata,
                                       variable = v,
                                       vcov = vcov,
                                       group_name = gn,
                                       prediction_type = predt,
                                       numDeriv_method = numDeriv_method,
                                       ...)
                if (length(group_names) > 1) {
                    tmp$group <- gn
                }
                tmp$term <- v
                tmp$type <- predt
                dydx <- c(dydx, list(tmp))
            }
        }
    }
    dydx <- dplyr::bind_rows(dydx)

    # an empty dydx data.frame may still be useful to display contrasts
    if (is.null(dydx)) {
        dydx <- data.frame()
    }

    # contrasts: logical and factor variables w/ emmeans
    cont <- list()
    for (predt in prediction_type) {
        for (v in variables$cont) {
            tmp <- try(get_contrast(model, v, type = predt), silent = TRUE)
            tmp$type <- predt
            cont <- c(cont, list(tmp))
        }
    }
    cont <- dplyr::bind_rows(cont)

    # output: return data only if there are numeric variables
    out <- dydx
    if (nrow(out) > 0) {  # no numeric variables
        if (isTRUE(return_data) && nrow(out) > 0) {
            newdata$rowid <- 1:nrow(newdata)
            out <- merge(out, newdata, by = "rowid")
        }
        stubcols <- c("rowid", "type", "group", "term", "dydx", "std.error",
                      sort(grep("^predicted", colnames(newdata), value = TRUE)))
        cols <- intersect(stubcols, colnames(out))
        cols <- unique(c(cols, colnames(out)))
        out <- out[, cols]
        if (all(out$group == "main")) {
            out$group <- NULL
        }
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
    class(out) <- c("marginaleffects", class(out))
    attr(out, "contrasts") <- cont
    attr(out, "prediction_type") <- prediction_type
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
