#' @export
get_pred_diff_logical <- function(model, 
                                  newdata, 
                                  variable, 
                                  prediction_type = "response",
                                  group_name = NULL) {

    # counterfactual dataset with TRUE:FALSE for every rowid
    at <- list()
    at[[variable]] <- c(TRUE, FALSE)
    cd <- counterfactual(data = newdata, at = at)
    pred <- get_predict.default(model, 
                                newdata = cd, 
                                prediction_type = prediction_type)
    if (is.matrix(pred) && !is.null(group_name)) {
        cd$dydx <- pred[, group_name, drop = TRUE]
    } else {
        cd$dydx <- pred
    }

    # compute difference: FALSE sorted before TRUE
    out <- cd[order(cd$rowid, cd[[variable]]), c("rowid", "dydx")] 
    out <- aggregate(dydx ~ rowid, data = out, FUN = diff, drop = FALSE)
    
    # output
    out$term <- variable
    if (is.null(group_name)) {
        out$group <- "main"
    } else {
        out$group <- group_name
    }
    return(out)
}
