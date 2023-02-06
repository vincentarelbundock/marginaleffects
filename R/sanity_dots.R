# This function is very strict.
sanity_dots <- function(model, calling_function = NULL, ...) {
    dots <- list(...)

    if (isTRUE(calling_function == "marginaleffects")) {
        # transform_pre: this would break `dydx` normalization
        # interaction: cross countrast+slope do not make sense
        # contrast_numeric: steer power users toward comparisons()
        # contrast_factor: steer power users toward comparisons()
        # transform_post: should we really be back-transforming slopes?
        unsupported <- c("transform_pre", "contrast_numeric", "contrast_factor", "transform_post", "cross")
        unsupported <- intersect(names(dots), unsupported)
        if (length(unsupported) > 0) {
            msg <- sprintf(
                "These arguments are supported by the `comparisons()` function but not by the `slopes()` function: %s",
                paste(unsupported, collapse = ", "))
            stop(msg, call. = FALSE)
        }
    }

    # deprecated
    if ("interaction" %in% names(dots)) {
        msg <- "The `interaction` argument has been deprecated. Please use `cross` instead."
        insight::format_warning(msg)
    }

    valid <- list()

    # mixed effects
    valid[["merMod"]] <- valid[["lmerMod"]] <- valid[["glmerMod"]] <- valid[["lmerModLmerTest"]] <-
        c("include_random", "re.form", "allow.new.levels", "random.only")
    valid[["brmsfit"]] <- c("ndraws", "re_formula", "allow_new_levels",
                            "sample_new_levels", "dpar", "resp")
    valid[["selection"]] <- c("part") # sampleSelection
    valid[["glmmTMB"]] <- c("re.form", "allow.new.levels", "zitype") # glmmTMB
    valid[["bam"]] <- c("exclude") # mgcv
    valid[["rlmerMod"]] <- c("re.form", "allow.new.levels")
    valid[["gamlss"]] <- c("what", "safe") #gamlss

    white_list <- c(
        "conf.int", "modeldata", "contrast_factor", "contrast_numeric", "internal_call", "df",
        "transform_post", "transform_pre", "ci_method", "side", "delta", "null", "equivalence", "draw",
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

    bad <- setdiff(names(dots), c(good, white_list))
    if (length(bad) > 0) {
        if (model_class %in% names(valid)) {
            msg <- sprintf("These arguments are not supported for models of class `%s`: %s. Valid arguments include: %s. Please file a request on Github if you believe that additional arguments should be supported: https://github.com/vincentarelbundock/marginaleffects/issues",
                           model_class, paste(bad, collapse = ", "), paste(valid[[model_class]], collapse = ", "))
        } else {
            msg <- sprintf("These arguments are not supported for models of class `%s`: %s. Please file a request on Github if you believe that additional arguments should be supported: https://github.com/vincentarelbundock/marginaleffects/issues",
                       model_class, paste(bad, collapse = ", "))
        }
        warning(msg, call. = FALSE)
    }
}

