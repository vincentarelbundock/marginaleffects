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
    if (
        !identical(class(model)[1], "geeglm") ||
            !isTRUE(type %in% c("response", "link"))
    ) {
        return(NULL)
    }
    response_scale <- identical(type, "response")
    jacobian_analytic_model_matrix(
        model = model,
        type = type,
        response_scale = response_scale,
        family = if (response_scale) stats::family(model) else NULL,
        ...
    )
}
