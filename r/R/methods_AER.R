#' @include set_coef.R
#' @rdname set_coef
#' @export
set_coef.tobit <- set_coef.default
# Note: needed because otherwise set.coef.survreg() would be called,
# which requires different treatment (see insight:::get_parameters.tobit()
# vs insight:::get_parameters.survreg())
