#' @include get_predict.R
#' @rdname @get_predict
#' @keywords internal
#' @export
get_predict.gamlss <- function(model,
                               newdata = insight::get_data(model),
                               vcov = FALSE,
                               conf_level = 0.95,
                               type = "response",
                               ...) {

    if (!isTRUE(checkmate::check_flag(vcov, null.ok = TRUE))) {
        msg <- "The `vcov` argument is not supported for models of this class."
        stop(msg, call. = FALSE)
    }

    # predict.gamlss() breaks when `newdata` includes unknown variables
    origindata <- insight::get_data(model)
    originvars <- colnames(origindata)
    setDF(newdata)
    tmp <- newdata[, originvars]
    out <- stats::predict(model, newdata = tmp, type = type, ...)

    if ("rowid" %in% colnames(newdata)) {
        out <- data.frame(rowid = newdata$rowid, predicted = out)
    } else {
        out <- data.frame(rowid = seq_along(out), predicted = out)
    }

    return(out)
}


#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.gamlss <- function(model, coefs) {
    # in gamlss
    if (length(coefs) == length(model$mu.coefficients)) {
        names(coefs) <- names(model$mu.coefficients)
    } else {
        stop("Unable not manipulate the coefficients of this model. Standard errors cannot be computed.", call. = FALSE)
    }
    model[["mu.coefficients"]] <- coefs
    return(model)
}


#' @rdname get_coef
#' @export
get_coef.gamlss <- function(model, ...) {
    stats::coef(model)
}

