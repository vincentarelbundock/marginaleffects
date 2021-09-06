#' @include get_dydx_glm.R
get_dydx.glmerMod <- get_dydx.glm

get_dydx_se.glmerMod <- function(...) {
stop("The `variance` argument is not supported for models of this class. The `variance` argument must be `NULL`.")
}

get_dydx.lmerMod <- get_dydx.glmerMod
get_dydx_se.lmerMod <- get_dydx_se.glmerMod
