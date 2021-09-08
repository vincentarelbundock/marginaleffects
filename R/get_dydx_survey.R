#' @include get_dydx_stats.R
#' @rdname get_gradient
#' @export
get_gradient.svyglm <- get_gradient.glm

#' @rdname reset_coefs
#' @export
reset_coefs.svyglm <- reset_coefs.glm

#' @rdname get_jacobian
#' @export
get_jacobian.svyglm <- get_jacobian.glm

#' @rdname get_dydx
#' @export
get_dydx.svyglm <- get_dydx.glm
