#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.glm <- function(model, coefs, ...) {
    # in glm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    ## But, there's an edge case!! When `predict(model, se.fit = TRUE)` is called without `newdata`, `predict.lm()` isn't called.
    ## Instead `model$linear.predictors` is returned directly if `type = "link"` and
    ## `model$fitted.values` is returned directly if `type = "response"`.
    ## `marginal_effects()` for "glm" is always called with `newdata`, so we won't hit this.
    model
}


#' @rdname set_coef
#' @export
set_coef.lm <- function(model, coefs, ...) {
    # in lm coefficients are named vector
    model[["coefficients"]][names(coefs)] <- coefs
    model
}


#' @rdname get_predict
#' @export
get_predict.lm <- function(model, newdata = insight::get_data(model), type = "response", ...) {
    MM <- attr(newdata, "marginaleffects_model_matrix")
    beta <- get_coef(model)
    if (!isTRUE(checkmate::check_matrix(MM)) || ncol(MM) != length(beta)) {
        out <- get_predict.default(model = model, newdata = newdata, type = type, ...)
        return(out)
    }
    p <- model$rank
    p1 <- seq_len(p)
    piv <- if (p) qr(model)$pivot[p1]
    if (!all(seq_len(ncol(MM)) %in% piv)) {
        MM <- MM[, piv, drop = FALSE]
        beta <- beta[piv]
    }
    if (getOption("marginaleffects_linalg", default = "RcppEigen") == "RcppEigen") {
        pred <- eigenMatMult(MM, beta)
    } else {
        pred <- drop(MM %*% beta)
    }
    
    # `pred` is a secret argument which re-uses the default get_predict to format a vector a data frame using correct `rowid`
    out <- get_predict.default(model = model, newdata = newdata, type = type, pred = pred, ...)
    return(out)
}


#' @rdname get_predict
#' @export
get_predict.glm <- function(model, newdata = insight::get_data(model), type = "response", ...) {
    out <- NULL
    MM <- attr(newdata, "marginaleffects_model_matrix")
    if (isTRUE(checkmate::check_matrix(MM))) {
        if (isTRUE(checkmate::check_choice(type, c("link")))) {
            out <- get_predict.lm(model = model, newdata = newdata, ...)
        } else if (isTRUE(checkmate::check_choice(type, "response")) || is.null(type)) {
            out <- get_predict.lm(model = model, newdata = newdata, ...)
            out$estimate <- stats::family(model)$linkinv(out$estimate)
        }
    }
    
    if (is.null(out)) {
        out <- get_predict.default(model = model, newdata = newdata, type = type, ...)
    }
    
    return(out)
}