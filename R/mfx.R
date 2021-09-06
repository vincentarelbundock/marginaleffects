get_dydx <- function (model, ...) {
    UseMethod("get_dydx", model)
}

get_dydx_se <- function (model, ...) {
    UseMethod("get_dydx_se", model)
}

#' compute marginal effects estimates using numerical derivatives
#' @export
mfx <- function(model, 
                variables = NULL, 
                fitfram = NULL, 
                group_names = NULL,
                variance = vcov(model)) {

    if (is.null(fitfram)) {
        fitfram <- insight::get_data(model)
    }

    if (is.null(variables)) {
        variables <- insight::find_variables(mod)$conditional
    }

    if (is.null(group_names)) group_names <- c(NULL)

    for (gn in group_names) {
        for (v in variables) {
            label <- ifelse(is.null(gn), sprintf("dydx_%s", v), sprintf("dydx_%s_%s", v, gn))
            tmp <- get_dydx(model = model, 
                            fitfram = fitfram,
                            variable = v,
                            group_name = gn)
            # strip attributes (multinom looks like a mess otherwise)
            tmp <- as.numeric(tmp)
            fitfram[[label]] <- tmp
        }
        if (!is.null(variance)) {
            for (v in variables) {
                label <- ifelse(is.null(gn), sprintf("se_dydx_%s", v), sprintf("dydx_%s_%s", v, gn))
                tmp <- get_dydx_se(model = model, 
                                   fitfram = fitfram,
                                   variable = v,
                                   variance = variance,
                                   group_name = gn)
                # strip attributes (multinom looks like a mess otherwise)
                fitfram[[label]] <- tmp
            }
        }
    }
    return(fitfram)
}
