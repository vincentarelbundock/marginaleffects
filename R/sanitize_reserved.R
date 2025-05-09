sanity_reserved <- function(model = NULL, modeldata = data.frame()) {
    if (is.null(model)) {
        return(invisible(NULL))
    }

    predictors <- insight::find_variables(model, flatten = TRUE)

    # reserved keywords
    # Issue #697: we used to allow "group", as long as it wasn't in
    # `variables`, but this created problems with automatic `by=TRUE`. Perhaps
    # I could loosen this, but there are many interactions, and the lazy way is
    # just to forbid entirely.
    # NOTE: if no modeldata is available, we use `newdata`, but that often has a
    # `rowid` column. This used to break the extensions.Rmd vignette.
    reserved <- c(
        "group", "term", "contrast", "estimate",
        "std.error", "statistic", "conf.low", "conf.high", "p.value",
        "p.value.nonsup", "p.value.noninf", "by")
    bad <- unique(intersect(c(names(predictors), colnames(modeldata)), reserved))

    if (length(bad) > 0) {
        msg <- c(
            "These variable names are forbidden to avoid conflicts with the outputs of `marginaleffects`:",
            toString(dQuote(bad, NULL)),
            "Please rename your variables before fitting the model.")
        insight::format_error(msg)
    }

    return(invisible(NULL))
}
