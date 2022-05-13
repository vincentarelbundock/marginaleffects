#' Extract posterior draws from a `predictions` or `marginaleffects` object derived from Bayesian models
#'
#' @param x An object produced by the `marginaleffects`, `comparisons`, or `predictions` functions
#' @return A data.frame with `drawid` and `draw` columns.
#' @export
posteriordraws <- function(x) {

    # long format can be very slow to merge, so we use `data.table`
    assert_dependency("data.table")

    # tidy.comparisons() saves draws in a nice format already
    draws <- attr(x, "posterior_draws")
    if (inherits(draws, "posterior_draws")) return(draws)

    if (!inherits(x, "marginaleffects") && !inherits(x, "predictions") && !inherits(x, "comparisons")) {
        warning('The `posteriordraws` function only supports objects of type "marginaleffects", "comparisons", or "predictions" produced by the `marginaleffects` package.',
                call. = FALSE)
        return(x)
    }
    if (is.null(attr(x, "posterior_draws"))) {
        warning('This object does not include a "posterior_draws" attribute. The `posteriordraws` function only supports bayesian models produced by the `marginaleffects` or `predictions` functions of the `marginaleffects` package.',
                call. = FALSE)
        return(x)
    }
    draws <- attr(x, "posterior_draws")
    if (nrow(draws) != nrow(x)) {
        stop('The number of parameters in the object does not match the number of parameters for which posterior draws are available.', call. = FALSE)
    }

    draws <- attr(x, "posterior_draws")
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
    setcolorder(out, cols)
    setDF(out)
    return(out)
}
