#' Method to raise model-specific warnings and errors
#'
#' @inheritParams marginaleffects
#' @return A warning, an error, or nothing
#' @rdname sanity_model_specific
#' @keywords internal
sanity_model_specific <- function (model,
                                   calling_function = "marginaleffects",
                                   ...) {
    UseMethod("sanity_model_specific", model)
}


#' @rdname sanity_model_specific
sanity_model_specific.default <- function(model,
                                          calling_function = "marginaleffects",
                                          ...) {
    dots <- list(...)
    # if (length(dots) > 0) {
    #     warning(sprintf("The following arguments will be ignored: %s. Please refer to the documentation for a list of supported model-specific arguments.", paste(sort(names(dots)), collapse = ", ")))
    # }
    return(invisible(NULL))
}


sanity_model_supported_class <- function(model) {
    supported <- list(
        "afex_aov",
        "betareg",
        "bife",
        "brglmFit",
        "brmsfit",
        c("bracl", "brmultinom", "brglmFit"),
        c("brnb", "negbin", "glm"),
        "clm",
        "coxph",
        "crch",
        "fixest",
        c("Gam", "glm", "lm"), # package: gam
        c("gam", "glm", "lm"), # package: mgcv
        c("geeglm", "gee", "glm"),
        "glm",
        "gls",
        "glmerMod",
        "glmrob",
        "glmmTMB",
        c("glmmPQL", "lme"),
        "glimML",
        "glmx",
        "hurdle",
        "hxlr",
        "ivreg",
        "iv_robust",
        "lm",
        "lmerMod",
        "lmerModLmerTest",
        "lmrob",
        "lmRob",
        "lm_robust",
        "loess",
        c("lrm", "lm"),
        c("lrm", "rms", "glm"),
        c("mblogit", "mclogit"),
        c("mclogit", "lm"),
        "mhurdle",
        "mlogit",
        c("multinom", "nnet"),
        c("negbin", "glm", "lm"),
        c("plm", "panelmodel"),
        "polr",
        "rlmerMod",
        "rq",
        c("scam", "glm", "lm"),
        c("selection", "selection", "list"),
        "speedglm",
        "speedlm",
        "stanreg",
        c("tobit", "survreg"),
        "tobit1",
        "truncreg",
        "zeroinfl")
    flag <- FALSE
    for (sup in supported) {
        if (!is.null(sup) && isTRUE(all(sup %in% class(model)))) {
            flag <- TRUE
        }
    }
    if (isFALSE(flag)) {
        support <- paste(sort(unique(sapply(supported, function(x) x[1]))), collapse = ", ")
        msg <-
'Models of class "%s" are not supported.

Supported model classes include: %s.

New modeling packages can usually be supported by `marginaleffects` if they include a working `predict()` method. If you believe that this is the case, please file a feature request on Github: https://github.com/vincentarelbundock/marginaleffects/issues'
        msg <- sprintf(msg, class(model)[1], support)
        stop(msg, call. = FALSE)
    }
}


sanitize_model <- function(model,
                           newdata,
                           ...) {

    # tidymodels appear to store the model fit in `model[["fit"]]`
    if (inherits(model, "model_fit") && "fit" %in% names(model)) {
        model <- model[["fit"]]
    }

    sanity_model_specific(model, newdata = newdata, ...)
    sanity_model_supported_class(model)
    return(model)
}
