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
get_prediction_jacobian_spec.brglmFit <- function(model, type, ...) {
    prediction_jacobian_spec_glm_family(model, "brglmFit", type)
}
