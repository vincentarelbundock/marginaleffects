#' @include get_dydx_glm.R
get_dydx.glmerMod <- get_dydx.glm

get_dydx_se.glmerMod <- function(...) {
stop("The `variance` argument is not supported for models of class `glmerMod`. The `variance` argument must be `NULL`.")
}
