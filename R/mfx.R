get_dydx <- function (model, ...) {
    UseMethod("get_dydx", model)
}

get_dydx_se <- function (model, ...) {
    UseMethod("get_dydx_se", model)
}

get_dydx.glm = function(model, fitfram, variable, ...) {
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- predict(model, newdata = fitfram_tmp, type = "response")
        return(pred)
    }
    out <- numDeriv::grad(func = inner, 
                          x = fitfram[[variable]], 
                          method = "simple")
    return(out)
}

get_dydx_se.glm <- function(model, fitfram, variable, variance, ...) {
    model_tmp <- model
    inner <- function(x) {
        model_tmp$coefficients <- x
        marginalfx <- get_dydx(model_tmp, 
                               fitfram = fitfram, 
                               variable = variable)
        return(marginalfx)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = coef(model), 
                            method = "simple")
    # Var(dydx) = J Var(beta) J'
    # reaches memory limit, so we only get the diagonal
    # computation trick: https://stackoverflow.com/a/42569902/342331
    A <- J %*% variance
    V <- colSums(t(A) * t(J))
    out <- sqrt(V)
    return(out)
}

#' compute marginal effects estimates using numerical derivatives
#' @export
mfx <- function(model, variables = NULL, fitfram = NULL, variance = vcov(model)) {
    if (is.null(fitfram)) {
        fitfram <- insight::get_data(model)
    }
    if (is.null(variables)) {
        variables <- insight::find_variables(mod)$conditional
    }
    for (v in variables) {
        fitfram[[sprintf("dydx_%s", v)]] <- get_dydx(
            model = model, 
            fitfram = fitfram,
            variable = v)
    }
    if (!is.null(variance)) {
        for (v in variables) {
            fitfram[[sprintf("se_dydx_%s", v)]] <- get_dydx_se(
                model = model, 
                fitfram = fitfram,
                variable = v,
                variance = variance)
        }
    }
    return(fitfram)
}


