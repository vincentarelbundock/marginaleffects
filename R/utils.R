find_categorical <- function(newdata = NULL, model = NULL) {
    out_newdata <- out_model <- NULL
    if (!is.null(newdata)) {
        idx <- sapply(newdata, function(newdata)
                      is.character(newdata) ||
                      is.factor(newdata) ||
                      is.logical(newdata) ||
                      isTRUE(attr(newdata, "factor")))
        out_newdata <- colnames(newdata)[idx]
    }
    # use terms to detect factor(), brms::mo(), survival::strata()
    if (!is.null(model)) {
        # terms: default
        te <- tryCatch(attr(stats::terms(model), "term.labels"), error = function(e) NULL)

        # terms: brmsfit objects do not have terms immediately available
        if (is.null(te)) {
            te <- tryCatch(attr(stats::terms(stats::formula(model)$formula), "term.labels"), error = function(e) NULL)
        }

        if (!is.null(te) && is.character(te)) {
            te <- te[grepl("^factor\\(|^mo\\(|^strata\\(", te)]
            # some functions like strata() take arguments, but that is difficult to support
            te <- te[!grepl(",", te)]
            te <- gsub("^(factor|mo|strata)\\((.*)\\)", "\\2", te)
            out_model <- te
        }
    }
    out <- c(out_newdata, out_model)
    return(out)
}
