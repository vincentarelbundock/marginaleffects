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


#' @rdname get_jacobian_analytic
#' @export
get_jacobian_analytic.lm <- function(model, type, ...) {
    if (
        !identical(class(model)[1], "lm") ||
            !isTRUE(type %in% c("response", "link"))
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


#' @rdname get_jacobian_analytic
#' @export
get_jacobian_analytic.glm <- function(model, type, ...) {
    if (
        !identical(class(model)[1], "glm") ||
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
