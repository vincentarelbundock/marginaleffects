#' @include get_dydx_glm.R
get_dydx.loess <- get_dydx.glm

get_dydx_se.loess <- function(...) {
stop("The `variance` argument is not supported for models of class `loess`. The `variance` argument must be `NULL`.")
}
