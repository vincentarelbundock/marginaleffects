#' @rdname get_model_matrix
#' @export
get_model_matrix.geeglm <- function(model, newdata, mfx = NULL) {
    # geepack does not define predict.geeglm(); prediction inherits the GLM
    # implementation and therefore uses the same model-matrix construction.
    get_model_matrix.glm(model, newdata, mfx = mfx)
}


#' @noRd
#' @export
get_prediction_jacobian_spec.geeglm <- function(model, type, ...) {
    prediction_jacobian_spec_glm_family(model, "geeglm", type)
}
