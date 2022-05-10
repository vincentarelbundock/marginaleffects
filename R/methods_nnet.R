#' @include sanity_model.R
#' @rdname sanity_model_specific
#' @export
sanity_model_specific.multinom <- function(model, calling_function = "marginaleffects", ...) {
    if (calling_function == "marginaleffects") {
        warning("The standard errors estimated by `marginaleffects` do not match those produced by Stata for `nnet::multinom` models. Please be very careful when interpreting the results.",
                call. = FALSE)
    }
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.multinom <- function(model, coefs) {
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



#' @include get_group_names.R
#' @rdname get_group_names
#' @export
get_group_names.multinom <- function(model, ...) {
    resp <- insight::get_response(model)
    if (is.factor(resp)) {
        out <- levels(resp)
    } else {
        out <- unique(resp)
    }
    return(out[2:length(out)])
}


#' @include get_predict.R
#' @rdname get_predict
#' @export
get_predict.multinom <- function(model,
                                 newdata = insight::get_data(model),
                                 vcov = FALSE,
                                 conf_level = 0.95,
                                 type = "probs",
                                 ...) {

    type <- sanity_type(model, type)

    # needed because `predict.multinom` uses `data` rather than `newdata`
    pred <- stats::predict(model,
                           newdata = newdata,
                           type = type,
                           ...)

    # atomic vector means there is only one row in `newdata`
    if (isTRUE(checkmate::check_atomic_vector(pred))) {
        pred <- matrix(pred, nrow = 1, dimnames = list(NULL, names(pred)))
    }

    # matrix with outcome levels as columns
    out <- data.frame(
        group = rep(colnames(pred), each = nrow(pred)),
        predicted = c(pred))

    # usually when `newdata` is supplied by `comparisons`
    if ("rowid" %in% colnames(newdata)) {
        out$rowid <- rep(newdata$rowid, times = ncol(pred))
    } else {
        out$rowid <- rep(seq_len(nrow(pred)), times = ncol(pred))
    }

    return(out)
}

