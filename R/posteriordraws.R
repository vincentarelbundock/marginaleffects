#' Extract posterior draws from a `predictions`, `comparisons`, or `marginaleffects` object derived from Bayesian models.
#'
#' @param x An object produced by the `marginaleffects`, `comparisons`, or `predictions` functions
#' @return A data.frame with `drawid` and `draw` columns.
#' @export
posteriordraws <- function(x) {

    # tidy.comparisons() sometimes already saves draws in a nice long format
    draws <- attr(x, "posterior_draws")
    if (inherits(draws, "posterior_draws")) return(draws)

    if (is.null(attr(x, "posterior_draws"))) {
        warning('This object does not include a "posterior_draws" attribute. The `posteriordraws` function only supports bayesian models produced by the `marginaleffects` or `predictions` functions of the `marginaleffects` package.',
                call. = FALSE)
        return(x)
    }

    if (nrow(draws) != nrow(x)) {
        stop('The number of parameters in the object does not match the number of parameters for which posterior draws are available.', call. = FALSE)
    }

    draws <- data.table(draws)
    setnames(draws, as.character(seq_len(ncol(draws))))

    for (v in colnames(x)) {
        draws[[v]] <- x[[v]]
    }

    out <- melt(
        draws,
        id.vars = colnames(x),
        variable.name = "drawid",
        value.name = "draw")

    cols <- unique(c("drawid", "draw", "rowid", colnames(out)))
    cols <- intersect(cols, colnames(out))
    setcolorder(out, cols)
    setDF(out)
    return(out)
}


average_draws <- function(data, index, draws, column, byfun = NULL) {
    insight::check_if_installed("collapse")
    w <- data[["marginaleffects_wts_internal"]]

    if (is.null(index)) {
        index <- intersect(colnames(data), "type")
    }

    if (length(index) > 0) {
        g <- collapse::GRP(data, by = index)

        if (is.null(byfun)) {
            draws <- collapse::fmean(
                draws,
                g = g,
                w = w,
                drop = FALSE)
        } else {
            draws <- collapse::BY(
                draws,
                g = g,
                FUN = byfun,
                drop = FALSE)
        }
        out <- data.table(
            g[["groups"]],
            average = apply(draws, 1, stats::median))

    } else {
        if (is.null(byfun)) {
            draws <- collapse::fmean(
                draws,
                w = w,
                drop = FALSE)
        } else {
            draws <- collapse::BY(
                draws,
                g = g,
                FUN = byfun,
                drop = FALSE)
        }
        out <- data.table(average = apply(draws, 1, stats::median))
    }

    setnames(out, old = "average", new = column)
    attr(out, "posterior_draws") <- draws
    return(out)
}