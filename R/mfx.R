get_dydx <- function (model, ...) {
    UseMethod("get_dydx", model)
}

get_dydx_se <- function (model, ...) {
    UseMethod("get_dydx_se", model)
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
