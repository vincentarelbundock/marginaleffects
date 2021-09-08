##############
#  glmerMod  #
##############

#' @include get_dydx_stats.R
#' @rdname get_gradient
#' @export
get_gradient.glmerMod <- get_gradient.glm

#' @rdname @reset_coefs
#' @export
reset_coefs.merMod <- function(model, coefs) {
    # in 'merMod', predictions work the slot called "beta", which is unnamed
    # `fixef(model)` returns the same thing named
    requireNamespace("methods")
    beta <- methods::slot(model, "beta")
    beta[match(names(coefs), names(lme4::fixef(model)))] <- as.numeric(coefs)
    methods::slot(model, "beta") <- beta
    model
}

#' @rdname get_jacobian
#' @export
get_jacobian.glmerMod <- get_jacobian.glm

#' @rdname get_dydx
#' @export
get_dydx.glmerMod <- get_dydx.glm

#############
#  lmerMod  #
#############

#' @rdname get_gradient
#' @export
get_gradient.lmerMod <- get_gradient.glm

#' @rdname reset_coefs
#' @export
reset_coefs.lmerMod <- reset_coefs.merMod

#' @rdname get_jacobian
#' @export
get_jacobian.lmerMod <- get_jacobian.glm

#' @rdname get_dydx
#' @export
get_dydx.lmerMod <- get_dydx.glm
