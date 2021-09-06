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

    # sanity checks and preparation
    if (is.null(fitfram)) {
        fitfram <- insight::get_data(model)
    }

    if (is.null(variables)) {
        variables <- insight::find_variables(model)$conditional
    }

    checkmate::assert_data_frame(fitfram)
    checkmate::assert_true(all(variables %in% colnames(fitfram)))

    if (!all(sapply(fitfram[, variables, drop = FALSE], is.numeric))) {
        stop("All the variables listed in the `variables` argument must be numeric.")
    }

    # computation
    if (is.null(group_names)) {
        counter <- 1
    } else {
        counter <- length(group_names)
    }

    for (i in seq(counter)) {
        for (v in variables) {
            label <- ifelse(is.null(group_names), 
                            sprintf("dydx_%s", v), 
                            sprintf("dydx_%s_%s", v, group_names[i]))
            tmp <- get_dydx(model = model, 
                            fitfram = fitfram,
                            variable = v,
                            group_name = group_names[i])
            # strip attributes (multinom looks like a mess otherwise)
            tmp <- as.numeric(tmp)
            fitfram[[label]] <- tmp
        }
        if (!is.null(variance)) {
            for (v in variables) {
                label <- ifelse(is.null(group_names), 
                                sprintf("se_dydx_%s", v), 
                                sprintf("dydx_%s_%s", v, group_names[i]))
                tmp <- get_dydx_se(model = model, 
                                   fitfram = fitfram,
                                   variable = v,
                                   variance = variance,
                                   group_name = group_names[i])
                # strip attributes (multinom looks like a mess otherwise)
                fitfram[[label]] <- tmp
            }
        }
    }
    return(fitfram)
}
