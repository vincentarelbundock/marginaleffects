#' compute marginal effects estimates using numerical derivatives
#' @export
marginaleffects <- function(model, 
                            newdata = NULL, 
                            variables = NULL, 
                            variance = TRUE,
                            numDeriv_method = "simple",
                            prediction_type = "response",
                            return_data = TRUE) {

    # sanity checks and pre-processing
    model <- sanity_dydx_model(model)
    newdata <- sanity_dydx_newdata(model, newdata)
    variables <- sanity_dydx_variables(model, newdata, variables)
    variance <- sanity_dydx_variance(model, variance)
    group_names <- sanity_dydx_group_names(model)
    prediction_type <- sanity_dydx_prediction_type(model, prediction_type)
    return_data <- sanity_dydx_return_data(return_data)

    # dydx: numeric variables w/ autodiff
    dydx <- list()
    for (gn in group_names) {
        for (v in variables$dydx) {
            tmp <- get_dydx_and_se(model = model, 
                                   fitfram = newdata,
                                   variable = v,
                                   variance = variance,
                                   group_name = gn,
                                   prediction_type = prediction_type,
                                   numDeriv_method = numDeriv_method)
            if (length(group_names) > 1) {
                tmp$group <- gn
            }
            tmp$term <- v
            dydx <- c(dydx, list(tmp))
        }
    }
    dydx <- poorman::bind_rows(dydx)

    # contrasts: logical and factor variables w/ emmeans
    cont <- list()
    for (v in variables$cont) {
        tmp <- try(get_contrast(model, v), silent = TRUE)
        cont <- c(cont, list(tmp))
    }
    cont <- poorman::bind_rows(cont)

    # clean output and merge original data
    out <- dydx
    newdata$rowid <- 1:nrow(newdata)
    if (isTRUE(return_data)) {
        dydx <- merge(dydx, newdata, by = "rowid")
    }
    cols <- intersect(c("rowid", "group", "term", "dydx", "std.error"), colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols]
    if (all(out$group == "main")) {
        out$group <- NULL
    }

    # attach model info
    if (check_dependency("modelsummary")) {
        gl <- try(modelsummary::get_gof(model), silent = TRUE)
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
