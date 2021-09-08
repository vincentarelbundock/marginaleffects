#' @include get_dydx_nnet.R
#' @rdname get_gradient
get_gradient.polr <- get_gradient.multinom

#' @rdname reset_coefs
reset_coefs.polr <- reset_coefs.default

#' @rdname get_jacobian
get_jacobian.polr <- get_jacobian.multinom

#' @include get_dydx_nnet.R
#' @rdname get_dydx
get_dydx.polr <- get_dydx.multinom
