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
        # grad() expects a vector output
        pred <- pred[, group_name, drop = TRUE]
        return(pred)
    }
    out <- numDeriv::grad(func = inner, 
                          x = fitfram[[variable]], 
                          method = numDeriv_method)
    # sometimes weird attributes must be stripped
    out <- as.numeric(out)
    return(out)
}




get_jacobian_multinom <- function(model, 
                                  fitfram, 
                                  variable, 
                                  variance = NULL, 
                                  group_name = NULL,
                                  prediction_type = "probs",
                                  numDeriv_method = "simple", 
                                  ...) {

    model_tmp <- model
    inner <- function(x) {
        model_tmp <- reset_coefs(model_tmp, x)
        g <- get_gradient_multinom(model = model_tmp, 
                                   fitfram = fitfram, 
                                   variable = variable, 
                                   group_name = group_name,
                                   prediction_type = prediction_type,
                                   numDeriv_method = numDeriv_method)
        return(g)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = as.vector(t(coef(model))),
                            method = numDeriv_method)
    J
}


get_dydx.multinom <- function(model, 
                              fitfram, 
                              variable, 
                              group_name,
                              variance, 
                              prediction_type = "probs",
                              numDeriv_method = "simple", 
                              ...) {

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


    # standard errors
    if (!is.null(variance)) {
        J <- get_jacobian_multinom(model = model,
                                   fitfram = fitfram,
                                   variable = variable,
                                   group_name = group_name,
                                   prediction_type = prediction_type,
                                   numDeriv_method = numDeriv_method)
        idx <- grepl(paste0("^", group_name, ":"), colnames(variance))
        out$std.error <- se_from_J_V(J, variance)
    }

    # output
    return(out)
}
