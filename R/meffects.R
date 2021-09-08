#' compute marginal effects estimates using numerical derivatives
#' @export
meffects <- function(model, 
                     newdata = NULL, 
                     at = NULL,
                     variables = NULL, 
                     variance = try(stats::vcov(model), silent = TRUE),
                     prediction_type = "response",
                     numDeriv_method = "simple") {

    # sanity checks and preparation
    model <- sanity_dydx_model(model)
    newdata <- sanity_dydx_at(model, newdata, at)
    newdata <- sanity_dydx_newdata(model, newdata)
    variables <- sanity_dydx_variables(model, newdata, variables)
    variance <- sanity_dydx_variance(model, variance)
    group_names <- sanity_dydx_group_names(model)
    prediction_type <- sanity_dydx_prediction_type(model, prediction_type)

    # computation
    out <- list()
    for (gn in group_names) {
        for (v in variables) {
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

    # clean output
    out <- do.call("rbind", out)
    newdata$rowid <- 1:nrow(newdata)
    out <- merge(out, newdata, by = "rowid")
    cols <- intersect(c("rowid", "group", "term", "dydx", "std.error"), colnames(out))
    cols <- unique(c(cols, colnames(out)))
    out <- out[, cols]
    if (is.character(out$group) && all(out$group == "main")) {
        out$group <- NULL
    }
    return(out)
}
