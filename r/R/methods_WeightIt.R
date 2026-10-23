#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.multinom_weightit <- function(model, ...) {
    stats::coef(model)
}


# The derivative of `glm_weightit` predictions with respect to the coefficients
# is the same as for `glm`. Any weight-estimation correction lives in `vcov()`
# and `estfun()`. WeightIt documents "probs" and "lp" as aliases of "response"
# and "link".
#' @noRd
#' @export
get_prediction_jacobian_spec.glm_weightit <- function(model, type, ...) {
    if (isTRUE(checkmate::check_string(type))) {
        type <- switch(type, probs = "response", lp = "link", type)
    }
    prediction_jacobian_spec_glm_family(model, "glm_weightit", type)
}
