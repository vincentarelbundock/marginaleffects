#' Method to raise model-specific warnings and errors
#'
#' @inheritParams slopes
#' @return A warning, an error, or nothing
#' @rdname sanitize_model_specific
#' @keywords internal
sanitize_model_specific <- function(model, ...) {
    UseMethod("sanitize_model_specific", model)
}


#' @rdname sanitize_model_specific
sanitize_model_specific.default <- function(
    model,
    vcov = NULL,
    calling_function = "marginaleffects",
    ...) {
    return(model)
}


sanity_model_supported_class <- function(model, custom = TRUE) {
    checkmate::assert_character(
        getOption("marginaleffects_model_classes", default = NULL),
        null.ok = TRUE
    )

    if (isTRUE(custom)) {
        custom_classes <- getOption("marginaleffects_model_classes", default = NULL)
        custom_classes <- as.list(custom_classes)
    } else {
        custom_classes <- NULL
    }

    supported <- append(
        custom_classes,
        list(
            "marginaleffects_internal",
            "afex_aov",
            "amest", # package: Amelia
            "bart", # package: dbarts
            "betareg",
            "bglmerMod",
            "blmerMod",
            # "bife",
            "biglm",
            c("bigglm", "biglm"),
            "brglmFit",
            "brmsfit",
            c("bracl", "brmultinom", "brglmFit"),
            c("brnb", "negbin", "glm"),
            c("clogit", "coxph"),
            "clm",
            c("clmm2", "clm2"),
            "coxph",
            "coxph_weightit",
            "crch",
            "flexsurvreg", # package: flexsurv
            "fixest",
            "flic",
            "flac",
            c("Gam", "glm", "lm"), # package: gam
            c("gam", "glm", "lm"), # package: mgcv
            c("gamlss", "gam", "glm", "lm"), # package: gamlss
            c("geeglm", "gee", "glm"),
            c("Gls", "rms", "gls"),
            "glm",
            "gls",
            "glmerMod",
            "glmrob",
            "glmmTMB",
            "glmgee",
            c("glmmPQL", "lme"),
            "glimML",
            "glmx",
            "glm_weightit",
            "hetprob",
            "hurdle",
            "hxlr",
            "ivreg",
            "iv_robust",
            "ivpml",
            "lda",
            "Learner",
            "lm",
            "lme",
            "lmerMod",
            "lmerModLmerTest",
            "lmrob",
            "lmRob",
            "lm_robust",
            "lmtree", # partykit
            "glmtree", # partykit
            # "logitr",
            "loess",
            "logistf",
            c("lrm", "lm"),
            c("lrm", "rms", "glm"),
            c("mblogit", "mclogit"),
            c("mclogit", "lm"),
            "MCMCglmm",
            "mhurdle",
            "mira",
            "mlogit",
            "model_fit",
            c("multinom", "nnet"),
            "multinom_weightit",
            "mvgam",
            c("negbin", "glm", "lm"),
            "nls",
            c("ols", "rms", "lm"),
            "ordinal_weightit",
            c("orm", "rms"),
            c("oohbchoice", "dbchoice"),
            "phylolm",
            "phyloglm",
            c("plm", "panelmodel"),
            "polr",
            "stpm2",
            "gsm",
            "pstpm2",
            "aft",
            "Rchoice",
            "rendo.base",
            "rlmerMod",
            "rq",
            c("scam", "glm", "lm"),
            c("selection", "selection", "list"),
            "speedglm",
            "speedlm",
            "stanreg",
            "survreg",
            "svyolr",
            "systemfit",
            c("tobit", "survreg"),
            "tobit1",
            "truncreg",
            "workflow",
            "zeroinfl"
        )
    )
    flag <- FALSE
    for (sup in supported) {
        if (!is.null(sup) && isTRUE(all(sup %in% class(model)))) {
            flag <- TRUE
        }
    }
    if (isFALSE(flag)) {
        support <- toString(sort(unique(sapply(supported, function(x) x[1]))))
        msg <- c(
            sprintf(
                'Models of class "%s" are not supported. Supported model classes include:',
                class(model)[1]
            ),
            "",
            support,
            "",
            "New modeling packages can usually be supported by `marginaleffects` if they include a working `predict()` method. If you believe that this is the case, please file a feature request on Github: https://github.com/vincentarelbundock/marginaleffects/issues"
        )
        msg <- insight::format_message(msg)
        stop(msg, call. = FALSE)
    }
}


# Function that operates on mfx objects
sanitize_model <- function(model, call, newdata = NULL, vcov = NULL, by = FALSE, ...) {
    # Extract calling_function from mfx@call
    calling_function <- extract_calling_function(call)

    # Sanitize the model
    model <- sanitize_model_specific(
        model,
        vcov = vcov,
        newdata = newdata,
        by = by,
        calling_function = calling_function,
        ...
    )
    sanity_model_supported_class(model)

    # Assign the sanitized model to the slot
    return(model)
}
