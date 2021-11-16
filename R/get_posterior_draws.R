#' Extract posterior draws from a `predictions` or `marginaleffects` object derived from Bayesian `brms` models
#'
#' @param x An object produced by the `predictions` or the `marginaleffects` functions
#' @return A data.frame with `drawid` and `draw` columns.
#' @export
get_posterior_draws <- function(x) {
    if (!inherits(x, "marginaleffects") && !inherits(x, "predictions")) {
        warning('The `get_posterior_draws` function only supports objects of type "marginaleffects" or "predictions" produced by the `marginaleffects` package.')
        return(x)
    }
    if (is.null(attr(x, "posterior_draws"))) {
        warning('This object does not include a "posterior_draws" attribute. The `get_posterior_draws` function only supports bayesian models produced by the `marginaleffects` or `predictions` functions of the `marginaleffects` package.')
        return(x)
    }
    draws <- attr(x, "posterior_draws")
    if (nrow(draws) != nrow(x)) {
        stop('The number of parameters in the object does not match the number of parameters for which posterior draws are available.')
    }
    out <- x
    out$rowid_internal <- 1:nrow(out)
    draws_df <- data.frame(
        rowid_internal = rep(1:nrow(out), by = ncol(draws)),
        drawid = rep(1:ncol(draws),  each = nrow(out)),
        draw = as.vector(draws))
    draws_df <- left_join(draws_df, out, by = "rowid_internal")
    draws_df$rowid_internal <- NULL
    return(draws_df)
}
