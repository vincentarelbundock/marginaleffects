###########################
#  default methods work:  #
#      get_gradient       #
#      get_coef           #
#      reset_coefs        #
#      get_jacobian       #
###########################


#' @rdname get_dydx
#' @export
get_dydx.polr <- function(model, 
                          fitfram, 
                          variable, 
                          group_name,
                          variance, 
                          prediction_type = "probs",
                          numDeriv_method = "simple", 
                          ...) {

    # marginal effects
    g <- get_gradient(model = model,
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
        J <- get_jacobian(model = model,
                          fitfram = fitfram,
                          variable = variable,
                          group_name = group_name,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        idx <- grepl(paste0("^", group_name, ":"), colnames(variance))
        V <- variance[names(stats::coef(model)),
                      names(stats::coef(model))]
        out$std.error <- se_from_J_V(J, V)
    }

    # output
    return(out)
}
