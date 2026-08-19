#' @rdname get_model_matrix
#' @export
get_model_matrix.geeglm <- function(model, newdata, mfx = NULL) {
    # geepack does not define predict.geeglm(); prediction inherits the GLM
    # implementation and therefore uses the same model-matrix construction.
    get_model_matrix.glm(model, newdata, mfx = mfx)
}


#' @noRd
#' @export
get_jacobian_analytic.geeglm <- function(model, type, ...) {
    jacobian_analytic_glm_family(model, "geeglm", type, ...)
}
