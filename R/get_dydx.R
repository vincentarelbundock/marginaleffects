get_dydx <- function(model,
                     variable,
                     group_name,
                     newdata,
                     type,
                     numDeriv_method) {

    if (variable %in% find_categorical(newdata) ||
        "brmsfit" %in% class(model)) {
        dydx_fun <- get_contrasts
    } else {
        dydx_fun <- get_dydx_continuous
    }

    out <- dydx_fun(model = model,
                    newdata = newdata,
                    v = variable,
                    group_name = group_name,
                    type = type,
                    numDeriv_method = numDeriv_method,
                    contrast_to_dydx = TRUE)

    # normalize names to merge when requesting dydx
    colnames(out)[colnames(out) == "contrast"] <- "dydx"

    return(out)
}

 get_dydx_continuous <- function(model, 
                                variable,
                                newdata = insight::get_data(model),
                                group_name = NULL,
                                type = "response",
                                numDeriv_method = "simple",
                                ...) {
    newdata_tmp <- newdata
    inner <- function(x) {
        newdata_tmp[[variable]] <- x
        pred <- get_predict(model = model,
                            newdata = newdata_tmp,
                            type = type,
                            group_name = group_name,
                            ...)

        # strip weird attributes added by some methods (e.g., predict.svyglm)
        pred <- as.numeric(pred)
        return(pred)
    }
    g <- numDeriv::grad(func = inner, 
                        x = newdata[[variable]], 
                        method = numDeriv_method)
    out <- data.frame(rowid = 1:nrow(newdata),
                      term = variable,
                      dydx = g)
    return(out)
}
