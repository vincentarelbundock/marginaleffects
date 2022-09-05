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
                                          vcov = NULL,
                                          calling_function = "marginaleffects",
                                          ...) {
    return(invisible(NULL))
}


sanity_model_supported_class <- function(model) {
    checkmate::assert_character(
        getOption("marginaleffects_model_classes", default = NULL),
        null.ok = TRUE)
    custom_classes <- getOption("marginaleffects_model_classes", default = NULL)
    custom_classes <- as.list(custom_classes)
    supported <- append(custom_classes, list(
        "afex_aov",
        "betareg",
        "bife",
        "biglm",
        c("bigglm", "biglm"),
        "brglmFit",
        "brmsfit",
        c("bracl", "brmultinom", "brglmFit"),
        c("brnb", "negbin", "glm"),
        c("clogit", "coxph"),
        "clm",
        "coxph",
        "crch",
        "fixest",
        c("Gam", "glm", "lm"), # package: gam
        c("gam", "glm", "lm"), # package: mgcv
        c("gamlss", "gam", "glm", "lm"), # package: gamlss
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
        "zeroinfl"))
    flag <- FALSE
    for (sup in supported) {
        if (!is.null(sup) && isTRUE(all(sup %in% class(model)))) {
            flag <- TRUE
        }
    }
    if (isFALSE(flag)) {
        support <- paste(sort(unique(sapply(supported, function(x) x[1]))), collapse = ", ")
        msg <- c(
            sprintf('Models of class "%s" are not supported. Supported model classes include:', class(model)[1]),
            "",
            support,
            "",
            "New modeling packages can usually be supported by `marginaleffects` if they include a working `predict()` method. If you believe that this is the case, please file a feature request on Github: https://github.com/vincentarelbundock/marginaleffects/issues")
        msg <- insight::format_message(msg)
        stop(msg, call. = FALSE)
    }
}


sanitize_model <- function(model,
                           newdata = NULL,
                           vcov = NULL,
                           ...) {

    # tidymodels appear to store the model fit in `model[["fit"]]`
    if (inherits(model, "model_fit") && "fit" %in% names(model)) {
        model <- model[["fit"]]
    }

    sanity_model_specific(model, vcov = vcov, newdata = newdata, ...)
    sanity_model_supported_class(model)
    return(model)
}
