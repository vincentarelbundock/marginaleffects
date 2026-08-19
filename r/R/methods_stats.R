#' @rdname get_model_matrix
#' @export
get_model_matrix.lm <- function(model, newdata, mfx = NULL) {
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


#' @rdname get_model_matrix
#' @export
get_model_matrix.glm <- get_model_matrix.lm


#' @noRd
#' @export
get_jacobian_analytic.lm <- function(model, type, ...) {
    jacobian_analytic_linear(model, "lm", type, c("response", "link"), ...)
}


#' @noRd
#' @export
get_jacobian_analytic.glm <- function(model, type, ...) {
    jacobian_analytic_glm_family(model, "glm", type, ...)
}
