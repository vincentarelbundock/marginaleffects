#' @rdname get_coef
#' @export
get_coef.brmultinom <- function(model, ...) {
    out <- insight::get_parameters(model)
    out <- stats::setNames(
        out$Estimate,
        sprintf("%s:%s", out$Response, out$Parameter)
    )
    return(out)
}


#' @include methods_nnet.R
#' @rdname get_predict
#' @export
get_predict.brmultinom <- get_predict.multinom


#' @include get_group_names.R
#' @include methods_nnet.R
#' @rdname get_group_names
#' @export
get_group_names.bracl <- get_group_names.multinom


#' @rdname get_coef
#' @export
get_coef.bracl <- function(model, ...) {
    stats::coef(model)
}


#' @rdname get_model_matrix
#' @export
get_model_matrix.brglmFit <- function(model, newdata, mfx = NULL) {
    get_model_matrix.glm(model, newdata, mfx = mfx)
}


#' @noRd
#' @export
get_jacobian_analytic.brglmFit <- function(model, type, ...) {
    if (
        !identical(class(model)[1], "brglmFit") ||
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
