#' @rdname get_predict
#' @export
get_predict.rq <- function(
    model,
    newdata = get_modeldata(model),
    type = NULL,
    ...) {
    # type argument of the method is used to specify confidence interval type
    insight::check_if_installed("quantreg")

    MM <- attr(newdata, "marginaleffects_model_matrix")
    if (isTRUE(checkmate::check_matrix(MM))) {
        beta <- get_coef(model)
        eta <- drop(MM %*% beta)
        if (isTRUE(checkmate::check_numeric(eta, len = nrow(newdata)))) {
            out <- data.table(estimate = eta)
        } else {
            out <- data.table(estimate = eta)
        }
        attr(out, "marginaleffects_linear_predictor") <- eta
        attr(out, "marginaleffects_model_matrix_used") <- TRUE
    } else {
        out <- quantreg::predict.rq(model, newdata = newdata, ...)
        out <- data.table(estimate = out)
    }

    out <- add_rowid(out, newdata)
    return(out)
}


#' @include sanity_model.R
#' @rdname sanitize_model_specific
#' @keywords internal
#' @export
sanitize_model_specific.rqs <- function(model, ...) {
    stop(
        "`marginaleffects` only supports `quantreg::rq` models with a single `tau` value.",
        call. = FALSE
    )
}

#' @rdname get_model_matrix
#' @export
get_model_matrix.rq <- function(model, newdata, mfx = NULL) {
    # Match predict.rq(): its predictions are X %*% coefficients.
    tt <- stats::delete.response(stats::terms(model))
    mf <- stats::model.frame(
        tt,
        data = newdata,
        na.action = stats::na.pass,
        xlev = model$xlevels
    )
    stats::model.matrix(
        tt,
        mf,
        contrasts.arg = model$contrasts
    )
}


#' @rdname get_jacobian_analytic
#' @export
get_jacobian_analytic.rq <- function(model, type, ...) {
    if (
        !identical(class(model)[1], "rq") ||
            !identical(type, "response")
    ) {
        return(NULL)
    }
    jacobian_analytic_model_matrix(
        model = model,
        type = type,
        response_scale = FALSE,
        ...
    )
}
