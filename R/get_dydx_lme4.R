##############
#  glmerMod  #
##############

#' @rdname @reset_coefs
#' @export
reset_coefs.merMod <- function(model, coefs) {
    # in 'merMod', predictions work the slot called "beta", which is unnamed
    # `fixef(model)` returns the same thing named
    beta <- methods::slot(model, "beta")
    beta[match(names(coefs), names(lme4::fixef(model)))] <- as.numeric(coefs)
    methods::slot(model, "beta") <- beta
    model
}


#############
#  lmerMod  #
#############
