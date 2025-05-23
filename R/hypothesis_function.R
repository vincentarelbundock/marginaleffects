hypothesis_function <- function(x, newdata, hypothesis, by) {
    draws <- attr(x, "posterior_draws")
    if (!is.null(draws)) {
        msg <- "The `hypothesis` argument does not support function for models with draws. You can use `get_draws()` to extract draws and manipulate them directly instead."
        insight::format_error(msg)
    }

    if ("rowid" %in% colnames(x) && "rowid" %in% colnames(newdata)) {
        x <- merge(
            x,
            newdata,
            all.x = TRUE,
            by = intersect(colnames(x), colnames(newdata))
        )
    } else if (isTRUE(nrow(x) == nrow(newdata))) {
        x <- cbind(x, newdata)
    }

    attr(x, "variables_datagrid") <- attr(newdata, "variables_datagrid")
    attr(x, "by") <- if (is.character(by)) by else names(by)

    argnames <- names(formals(hypothesis))
    if (!"x" %in% argnames)
        insight::format_error(
            "The `hypothesis` function must accept an `x` argument."
        )
    if (!all(argnames %in% c("x", "draws"))) {
        msg <- "The allowable arguments for the `hypothesis` function are: `x` and `draws`"
        insight::format_error(msg)
    }
    args <- list(x = x, newdata = newdata, by = by, draws = draws)
    args <- args[names(args) %in% argnames]
    out <- do.call(hypothesis, args)
    at <- attr(out, "hypothesis_function_by")

    # sanity
    msg <- "The `hypothesis` argument function must return a data frame with `term` (or `hypothesis`) and `estimate` columns."
    if (inherits(out, "data.frame")) {
        if (
            !all(c("term", "estimate") %in% colnames(out)) &&
                !all(c("hypothesis", "estimate") %in% colnames(out))
        ) {
            insight::format_error(msg)
        }
    } else if (isTRUE(checkmate::check_numeric(out))) {
        if (
            isTRUE(checkmate::check_data_frame(x, nrows = length(out))) &&
                "term" %in% colnames(out)
        ) {
            out <- data.frame(term = out$term, estimate = out)
        } else {
            out <- data.frame(term = seq_along(out), estimate = out)
        }
    } else {
        insight::format_error(msg)
    }

    data.table::setDT(out)
    attr(out, "hypothesis_function_by") <- at
    return(out)
}
