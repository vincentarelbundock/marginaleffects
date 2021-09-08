#' @include get_dydx_stats.R
#' @rdname get_gradient
#' @export
get_gradient.ivreg <- get_gradient.glm

#' @rdname reset_coefs
#' @export
reset_coefs.ivreg <- reset_coefs.default

#' @rdname get_jacobian
#' @export
get_jacobian.ivreg <- get_jacobian.glm

#' @rdname get_dydx
#' @export
get_dydx.ivreg <- get_dydx.glm
