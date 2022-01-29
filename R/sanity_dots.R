# This function is very strict.
sanity_dots <- function(model, ...) {
    valid <- list(
        "merMod" = c("include_random", "re.form", "allow.new.levels", "random.only"),
        "lmerMod" = c("include_random", "re.form", "allow.new.levels", "random.only"),
        "glmerMod" = c("include_random", "re.form", "allow.new.levels", "random.only"),
        "lmerModLmerTest" = c("include_random", "re.form", "allow.new.levels", "random.only"),
        "glmerModLmerTest" = c("include_random", "re.form", "allow.new.levels", "random.only"),
        "brmsfit" = c("ndraws", "re_formula", "allow_new_levels", "sample_new_levels")
    )

    white_list <- c("conf.int")

    dots <- list(...)
    model_class <- class(model)[1]

    good <- NULL
    if (model_class %in% names(valid)) {
        good <- valid[[model_class]]
    }

    bad <- setdiff(names(dots), c(good, white_list))
    if (length(bad) > 0) {
        if (model_class %in% names(valid)) {
            msg <- sprintf("These arguments are not supported for models of class %s: %s. Valid arguments include: %s. Please file a request on Github if you believe that additional arguments should be supported: https://github.com/vincentarelbundock/marginaleffects/issues",
                           model_class, paste(bad, collapse = ", "), paste(valid[[model_class]], collapse = ", "))
        } else {
            msg <- sprintf("These arguments are not supported for models of class %s: %s. Please file a request on Github if you believe that additional arguments should be supported: https://github.com/vincentarelbundock/marginaleffects/issues",
                       model_class, paste(bad, collapse = ", "))
        }
        stop(msg)
    }
}
