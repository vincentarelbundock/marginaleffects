#' @rdname get_model_matrix
#' @export
get_model_matrix.ivreg <- function(model, newdata, mfx = NULL) {
    # Match predict.ivreg(type = "response") exactly: build the model frame
    # with the full formula, then the matrix from the stage-two regressors.
    tt_full <- stats::delete.response(stats::terms(model, component = "full"))
    mf <- stats::model.frame(
        tt_full,
        data = newdata,
        na.action = stats::na.pass,
        xlev = model$levels
    )
    tt_regressors <- stats::delete.response(
        stats::terms(model, component = "regressors")
    )
    stats::model.matrix(
        tt_regressors,
        data = mf,
        contrasts.arg = model$contrasts$regressors
    )
}


#' @noRd
#' @export
get_jacobian_analytic.ivreg <- function(model, type, ...) {
    jacobian_analytic_linear(model, "ivreg", type, "response", ...)
}
