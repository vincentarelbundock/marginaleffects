#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.multinom <- function(model, coefs) {
    # internally, coefficients are held in the `wts` vector, with 0s
    # interspersed. When transforming that vector to a matrix, we see that the
    # first row and first column are all zeros. 
    # NOTE: must use `newdata` in predict otherwise returns stored object.
    b_original <- get_coef(model)
    model$wts[match(b_original, model$wts)] <- coefs
    return(model)
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.multinom <- function(model, ...) {
    out <- insight::get_parameters(model, ...)
    out <- stats::setNames(
        out$Estimate,
        sprintf("%s:%s", out$Response, out$Parameter))
    return(out)
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

    type <- sanitize_type(model, type)

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

