hypothesis_function <- function(x, newdata, hypothesis, by) {
    # Posterior draws never reach this point: the plan rejects function
    # hypotheses for models with draws (see `plan_draws_check_hypothesis()`).
    draws <- NULL

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

    argnames <- names(formals(hypothesis))
    if (!"x" %in% argnames) {
        stop_sprintf(
            "The `hypothesis` function must accept an `x` argument."
        )
    }
    if (!all(argnames %in% c("x", "newdata", "by", "draws"))) {
        msg <- "The allowable arguments for the `hypothesis` function are: `x`, `newdata`, `by`, and `draws`"
        stop_sprintf(msg)
    }
    args <- list(x = x, newdata = newdata, by = by, draws = draws)
    args <- args[names(args) %in% argnames]
    out <- do.call(hypothesis, args)

    # sanity
    msg <- "The `hypothesis` argument function must return a data frame with `term` (or `hypothesis`) and `estimate` columns."
    if (length(out) == 0 || (inherits(out, "data.frame") && nrow(out) == 0)) {
        stop_sprintf(
            "The `hypothesis` function returned an empty result. When `hypotheses()` is called on a model (rather than on a `marginaleffects` object), the `x` argument is the model itself, so `x$estimate` is `NULL`; use `coef(x)` instead."
        )
    }
    if (inherits(out, "data.frame")) {
        if (
            !all(c("term", "estimate") %in% colnames(out)) &&
                !all(c("hypothesis", "estimate") %in% colnames(out))
        ) {
            stop_sprintf(msg)
        }
    } else if (isTRUE(checkmate::check_numeric(out))) {
        if (
            isTRUE(checkmate::check_data_frame(x, nrows = length(out))) &&
                "term" %in% colnames(x)
        ) {
            out <- data.frame(term = x$term, estimate = out)
        } else {
            out <- data.frame(term = seq_along(out), estimate = out)
        }
    } else {
        stop_sprintf(msg)
    }

    return(out)
}
