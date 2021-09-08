#' @rdname get_gradient
#' @export
get_gradient.multinom <- function(model, 
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


#' @include reset_coefs.R
#' @rdname reset_coefs
#' @export
reset_coefs.multinom <- function(model, coefs) {
    # internally, coefficients are held in the `wts` vector, with 0s
    # interspersed. When transforming that vector to a matrix, we see that the
    # first row and first column are all zeros. 
    # NOTE: must use `newdata` in predict otherwise returns stored object.
    coefs <- matrix(coefs, nrow = model$n[3L] - 1)
    coefs <- rbind(rep(0, ncol(coefs)), coefs)
    coefs <- cbind(rep(0, nrow(coefs)), coefs)
    model$wts <- as.vector(t(coefs))
    return(model)
}


#' @rdname get_jacobian
#' @export
get_jacobian.multinom <- function(model, 
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
        g <- get_gradient(model = model_tmp, 
                          fitfram = fitfram, 
                          variable = variable, 
                          group_name = group_name,
                          prediction_type = prediction_type,
                          numDeriv_method = numDeriv_method)
        return(g)
    }
    J <- numDeriv::jacobian(func = inner, 
                            x = as.vector(t(stats::coef(model))),
                            method = numDeriv_method)
    J
}

#' @rdname get_dydx
#' @export
get_dydx.multinom <- function(model, 
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
        out$std.error <- se_from_J_V(J, variance)
    }

    # output
    return(out)
}
