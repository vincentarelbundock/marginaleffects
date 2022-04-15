#' Extract posterior draws from a `predictions` or `marginaleffects` object derived from Bayesian models
#'
#' @param x An object produced by the `marginaleffects`, `comparisons`, or `predictions` functions
#' @return A data.frame with `drawid` and `draw` columns.
#' @export
posteriordraws <- function(x) {

    # long format can be very slow to merge, so we use `data.table`
    assert_dependency("data.table")

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
        stop('The number of parameters in the object does not match the number of parameters for which posterior draws are available.')
    }

    draws <- attr(x, "posterior_draws")
    draws <- data.table(draws)
    setnames(draws, as.character(seq_len(ncol(draws))))
    idx <- intersect(colnames(x), c("rowid", "term", "group", "contrast", "type"))
    for (i in idx) {
        draws[, (i) := x[[i]]]
    }

    out <- melt(
        draws,
        id.vars = idx,
        variable.name = "drawid",
        value.name = "draw")

    setDF(out)

    return(out)
}
