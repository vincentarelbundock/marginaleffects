#' @include set_coef.R
#' @rdname set_coef
#' @keywords internal
#' @export
set_coef.merMod <- function(model, coefs, ...) {
    # in 'merMod', predictions work the slot called "beta", which is unnamed
    # `fixef(model)` returns the same thing named
    beta <- methods::slot(model, "beta")
    beta[match(names(coefs), names(lme4::fixef(model)))] <- as.numeric(coefs)
    methods::slot(model, "beta") <- beta
    model
}


#' @include get_coef.R
#' @rdname get_coef
#' @export
get_coef.merMod <- function(model, ...) {
    lme4::fixef(model)
}


#' @rdname get_predict
#' @export
get_predict.merMod <- function(
    model,
    newdata = insight::get_data(model),
    type = "response",
    ...
) {
    get_predict.default(model, newdata = newdata, type = type, ...)
}


#' @rdname set_coef
#' @export
set_coef.lmerModLmerTest <- set_coef.merMod


#' @rdname get_coef
#' @export
get_coef.lmerModLmerTest <- get_coef.merMod


#' @rdname get_predict
#' @export
get_predict.lmerModLmerTest <- get_predict.merMod


#' @rdname set_coef
#' @export
set_coef.lmerMod <- set_coef.merMod


#' @rdname get_coef
#' @export
get_coef.lmerMod <- get_coef.merMod


#' @rdname get_predict
#' @export
get_predict.lmerMod <- get_predict.merMod


#' @rdname sanitize_model_specific
#' @export
sanitize_model_specific.merMod <- function(model, re.form, ...) {
    if (missing(re.form) || (!isTRUE(is.na(re.form)))) {
        msg <- "For this model type, `marginaleffects` only takes into account the uncertainty in fixed-effect parameters. This is often appropriate when `re.form=NA`, but may be surprising to users who set `re.form=NULL` (default) or to some other value. Call `options(marginaleffects_safe = FALSE)` to silence this warning."
        warn_sprintf(msg)
    }
    return(model)
}
