#' @include get_dydx_stats.R
get_gradient_multinom <- function(model, 
                                  fitfram, 
                                  variable, 
                                  group_name = NULL,
                                  prediction_type = "probs",
                                  numDeriv_method = "simple") {
    fitfram_tmp <- fitfram
    inner <- function(x) {
        fitfram_tmp[[variable]] <- x
        pred <- stats::predict(model, 
                               newdata = fitfram_tmp, 
                               type = prediction_type)
        if (!group_name %in% colnames(pred)) {
            stop(sprintf('"%s" does not appear to be a valid `group_names` for this model. Make sure it is a level of the response variable.', group_name))
        }
        pred <- pred[, group_name, drop = FALSE]
        return(pred)
    }
    out <- numDeriv::grad(func = inner, 
                          x = fitfram[[variable]], 
                          method = numDeriv_method)
    # sometimes weird attributes rear their heads
    out <- as.numeric(out)
    return(out)
}

get_dydx.multinom <- function(model, 
                              fitfram, 
                              variable, 
                              group_name,
                              variance, 
                              prediction_type = "probs",
                              numDeriv_method = "simple", 
                              ...) {

    if (is.null(group_name)) {
        stop('You must specify the `group_names` for which to calculate marginal effects. These names are typically the factor levels of the response variable.')
    }

    if (prediction_type != "probs") {
        stop('The only `prediction_type` supported for models of class `multinom` is `"probs"`.')
    }

    # marginal effects
    g <- get_gradient_multinom(model = model,
                               fitfram = fitfram,
                               variable = variable,
                               group_name = group_name,
                               prediction_type = prediction_type,
                               numDeriv_method = numDeriv_method)
    out <- data.frame(rowid = 1:nrow(fitfram),
                      group = group_name, 
                      term = variable, 
                      dydx = g)

    if (!is.null(variance)) {
        stop("Variance estimates for models of this class are not supported yet. Please set `variance=NULL`")
    }

    return(out)
}
