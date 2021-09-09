#' compute marginal effects estimates using numerical derivatives
#' @export
marginaleffects <- function(model, 
                            newdata = NULL, 
                            variables = NULL, 
                            variance = try(stats::vcov(model), silent = TRUE),
                            return_data = TRUE,
                            prediction_type = "response",
                            numDeriv_method = "simple") {

    # sanity checks and preparation
    model <- sanity_dydx_model(model)
    newdata <- sanity_dydx_newdata(model, newdata)
    variables <- sanity_dydx_variables(model, newdata, variables)
    variance <- sanity_dydx_variance(model, variance)
    group_names <- sanity_dydx_group_names(model)
    prediction_type <- sanity_dydx_prediction_type(model, prediction_type)

    #################################
    #  numeric variables: autodiff  #
    #################################

    out <- list()
    for (gn in group_names) {
        for (v in variables$numeric) {
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
            out <- c(out, list(tmp))
        }
    }

    out <- poorman::bind_rows(out)

    ###############################################################
    #  logical variables: differences in predicted probabilities  #
    ###############################################################

    out_logical <- list()
    for (gn in group_names) {
        for (v in variables$logical) {
            tmp <- get_pred_diff_logical(model = model, 
                                         newdata = newdata, 
                                         variable = v, 
                                         group_name = NULL)
        out_logical <- c(out_logical, list(tmp))
        }
    }
    out_logical <- poorman::bind_rows(out_logical)

    out <- poorman::bind_rows(out, out_logical)

    # clean output and merge original data
    newdata$rowid <- 1:nrow(newdata)
    out <- merge(out, newdata, by = "rowid")
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
    attr(out, "prediction_type") <- prediction_type
    attr(out, "numDeriv_method") <- numDeriv_method
    attr(out, "model_type") <- class(model)[1]

    return(out)
}
