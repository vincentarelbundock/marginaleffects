# This function is very strict.
sanity_dots <- function(model, calling_function = NULL, ...) {
    stop_deprecate <- c()
    if ("p_adjust" %in% ...names()) stop_deprecate("p_adjust", "inferences()")
    if ("transform_post" %in% ...names()) stop_deprecate("transform_post", "transform")
    if ("interaction" %in% ...names()) stop_deprecate("interaction", "cross")

    if (identical(calling_function, "slopes")) {
        if ("comparison" %in% ...names()) stop_deprecate("comparison")
        if ("transform" %in% ...names()) stop_deprecate("transform")
        if ("cross" %in% ...names()) stop_deprecate("cross")
    }

    valid <- list()

    # mixed effects
    me <- c("include_random", "re.form", "allow.new.levels", "random.only")
    valid[["merMod"]] <- me
    valid[["lmerMod"]] <- me
    valid[["glmerMod"]] <- me
    valid[["lmerModLmerTest"]] <- me

    # bayes
    valid[["brmsfit"]] <- c(
        "draw_ids",
        "nlpar",
        "ndraws",
        "re_formula",
        "allow_new_levels",
        "sample_new_levels",
        "dpar",
        "resp"
    )
    valid[["brmsfit_multiple"]] <- valid[["brmsfit"]]

    # misc
    valid[["selection"]] <- c("part") # sampleSelection
    valid[["glmmTMB"]] <- c("re.form", "allow.new.levels", "zitype") # glmmTMB
    valid[["bam"]] <- c("exclude", "discrete") # mgcv
    valid[["gam"]] <- c("exclude", "discrete") # mgcv
    valid[["rlmerMod"]] <- c("re.form", "allow.new.levels")
    valid[["gamlss"]] <- c("what", "safe") # gamlss
    valid[["lme"]] <- c("level") # nlme::lme
    valid[["bife"]] <- c("alpha_new", "corrected") # nlme::lme
    valid[["process_error"]] <- c("times", "p", "start")
    valid[["flexsurvreg"]] <- c("times", "p", "start")

    # survival
    valid[["survreg"]] <- c("p")

    # WeightIt models
    valid[["ordinal_weightit"]] <- valid[["multinom_weightit"]] <- "values"

    white_list <- c(
        "conf.int",
        "modeldata",
        "internal_call",
        "df",
        "transform",
        "comparison",
        "side",
        "delta",
        "null",
        "equivalence",
        "draw",
        "flag", # internal dev
        "variables_grid", # backward compatibility in marginal_means()
        "at" # topmodels procast
    )

    model_class <- class(model)[1]

    good <- NULL
    if (model_class %in% names(valid)) {
        good <- valid[[model_class]]
    }

    backward_compatibility <- c("conf.level")
    good <- c(good, backward_compatibility)

    bad <- setdiff(...names(), c(good, white_list))
    if (length(bad) > 0) {
        if (model_class %in% names(valid)) {
            msg <- sprintf(
                "These arguments are not known to be supported for models of class `%s`: %s. These arguments are known to be valid: %s. All arguments are still passed to the model-specific prediction function, but users are encouraged to check if the argument is indeed supported by their modeling package. Please file a request on Github if you believe that an unknown argument should be added to the `marginaleffects` white list of known arguments, in order to avoid raising this warning: https://github.com/vincentarelbundock/marginaleffects/issues",
                model_class,
                toString(bad),
                toString(valid[[model_class]])
            )
        } else {
            msg <- sprintf(
                "These arguments are not known to be supported for models of class `%s`: %s. All arguments are still passed to the model-specific prediction function, but users are encouraged to check if the argument is indeed supported by their modeling package. Please file a request on Github if you believe that an unknown argument should be added to the `marginaleffects` white list of known arguments, in order to avoid raising this warning: https://github.com/vincentarelbundock/marginaleffects/issues",
                model_class,
                toString(bad)
            )
        }
        warning(msg, call. = FALSE)
    }
}
