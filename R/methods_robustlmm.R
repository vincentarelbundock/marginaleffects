#' @rdname set_coef
#' @export
set_coef.rlmerMod <- function(model, coefs) {
    model@beta <- coefs
    model
}


#' @rdname get_predict
#' @export
get_predict.rlmerMod <- function(model,
                                 newdata = insight::get_data(model),
                                 ...) {
    args <- list(...)
    # some predict methods raise warnings on unused arguments
    unused <- c("type", "normalize_dydx", "step_size", "numDeriv_method", "conf.level")
    args <- args[setdiff(names(args), unused)]
    args[["object"]] <- model
    args[["newdata"]] <- newdata
    out <- data.frame(rowid = seq_len(nrow(newdata)),
                      predicted = do.call("predict", args))
    return(out)
}
