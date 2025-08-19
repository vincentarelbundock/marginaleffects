equivalence <- function(x, equivalence = NULL, df = Inf, draws = NULL, ...) {
    if (is.null(equivalence)) {
        return(x)
    }

    if (!is.null(draws)) {
        # `draws` is an Parameters x Draws matrix.
        # index of draws in the equivalence interval
        idx_equiv <- draws > equivalence[1] & draws < equivalence[2]

        # share of the posterior draws in the equivalence interval
        x$p.equivalence <- apply(idx_equiv, 1, mean)

        ## TODO: check this calculation
        # # index of draws in the credible interval
        # idx_ci <- draws > x$conf.low & draws < x$conf.high
        # # rope only considers draws in the credible interval
        # # we NA them before taking the mean
        # idx_rope <- idx_equiv
        # idx_rope[!idx_ci] <- FALSE
        # x$rope <- apply(idx_rope, 1, mean, na.rm = TRUE)

        return(x)
    }

    if (!all(c("estimate", "std.error") %in% colnames(x))) {
        msg <- "The `equivalence` argument is not supported with models for which `marginaleffects` does not estimate a standard error."
        stop_sprintf(msg)
    }

    checkmate::assert_numeric(equivalence, min.len = 1, max.len = 2)
    if (length(equivalence) == 1) {
        equivalence <- c(equivalence, equivalence)
    }

    delta <- abs(diff(equivalence)) / 2

    # definitions from `emmeans`, with a different user interface based on symmetric "equivalence"
    x$statistic.noninf <- (x$estimate - equivalence[1]) / x$std.error
    x$statistic.nonsup <- (x$estimate - equivalence[2]) / x$std.error

    ## keep this in case we return to the emmeans-style user interface
    # x$statistic.inf <- (x$estimate - null + delta) / x$std.error
    # x$statistic.sup <- (x$estimate - null - delta) / x$std.error

    if (is.infinite(df)) {
        x$p.value.noninf <- stats::pnorm(x$statistic.noninf, lower.tail = FALSE)
        x$p.value.nonsup <- stats::pnorm(x$statistic.nonsup, lower.tail = TRUE)
    } else {
        x$p.value.noninf <- stats::pt(
            x$statistic.noninf,
            lower.tail = FALSE,
            df = x[["df"]]
        )
        x$p.value.nonsup <- stats::pt(
            x$statistic.nonsup,
            lower.tail = TRUE,
            df = x[["df"]]
        )
    }
    x$p.value.equiv <- pmax(x$p.value.nonsup, x$p.value.noninf)

    return(x)
}
