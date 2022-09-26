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
    # use terms to detect factor(), as.factor(), as.logical(), brms::mo(), survival::strata(), fixest::i()
    if (!is.null(model)) {
        # terms: default
        te <- tryCatch(attr(stats::terms(model), "term.labels"), error = function(e) NULL)

        # terms: brmsfit objects do not have terms immediately available
        if (is.null(te)) {
            te <- tryCatch(attr(stats::terms(stats::formula(model)$formula), "term.labels"), error = function(e) NULL)
        }

        if (!is.null(te) && is.character(te)) {

            # fixest::i()
            fi <- NULL
            idx <- grepl("^i\\(.*\\)$", te)
            if (sum(idx) > 0) {
                arg1 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[2]])))
                arg2 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[3]])))
                arg1 <- unlist(arg1, recursive = TRUE)
                arg2 <- unlist(arg2, recursive = TRUE)
                arg2 <- gsub("^i\\.", "", arg2[grepl("^i\\.", arg2)])
                fi <- unique(c(arg1, arg2))
            }

            te <- te[grepl("^as\\.factor|^factor\\(|^as.logical\\(|^mo\\(|^strata\\(", te)]
            # some functions like strata() take arguments, but that is difficult to support
            te <- gsub("^(^as\\.factor|^factor|^as\\.logical|mo|strata)\\((.*)\\)", "\\2", te)

            out_model <- unique(c(te[!grepl(",", te)], fi))
        }
    }

    out <- c(out_newdata, out_model)
    return(out)
}
