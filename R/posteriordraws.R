#' Extract posterior draws from a `predictions` or `marginaleffects` object derived from Bayesian `brms` models
#'
#' @param x An object produced by the `predictions` or the `marginaleffects` functions
#' @return A data.frame with `drawid` and `draw` columns.
#' @export
posteriordraws <- function(x) {

    # long format can be very slow to merge, so we use `data.table`
    assert_dependency("data.table")

    if (!inherits(x, "marginaleffects") && !inherits(x, "predictions")) {
        warning('The `posteriordraws` function only supports objects of type "marginaleffects" or "predictions" produced by the `marginaleffects` package.',
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

    out <- x
    idx <- lapply(unique(out$type), function(x) out$type == x)
    names(idx) <- unique(out$type)
    draws_by_type <- lapply(idx, function(x) draws[x, , drop = FALSE])
    out_by_type <- lapply(idx, function(x) out[x, , drop = FALSE])
    out_by_type <- lapply(out_by_type, function(x) transform(x, rowid_internal = 1:nrow(x)))
    out <- data.table::rbindlist(out_by_type)

    reshape_draws <- function(k) {
        z <- draws_by_type[[k]]
        data.frame(
            type = k,
            rowid_internal = rep(1:nrow(z), by = ncol(z)),
            drawid = rep(1:ncol(z),  each = nrow(z)),
            draw = as.vector(z))
    }
    draws_by_type <- lapply(names(draws_by_type), reshape_draws)
    draws_df <- data.table::rbindlist(draws_by_type)

    out <- left_join(draws_df, out, all.x = TRUE, by = c("type", "rowid_internal"))

    # always return a data.frame, even when data.table is used to speed things up
    out <- as.data.frame(out)

    return(out)
}
