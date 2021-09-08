#' @include get_dydx_stats.R
#' @rdname get_gradient
#' @export
get_gradient.fixest <- get_gradient.glm

#' @rdname reset_coefs
#' @export
reset_coefs.fixest <- reset_coefs.default

#' @rdname get_jacobian
#' @export
get_jacobian.fixest <- get_jacobian.glm

#' @rdname get_dydx
#' @export
get_dydx.fixest <- get_dydx.glm
