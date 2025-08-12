equivalence <- function(x, equivalence = NULL, df = Inf, ...) {
    if (is.null(equivalence)) {
        return(x)
    }

    if (!is.null(equivalence) && !all(c("estimate", "std.error") %in% colnames(x))) {
        msg <- "The `equivalence` argument is not supported with models for which `marginaleffects` does not estimate a standard error (e.g., bayesian)."
        stop_sprintf(msg)
    }

    checkmate::assert_numeric(equivalence, min.len = 1, max.len = 2)
    if (length(equivalence) == 1) {
        equivalence <- c(equivalence, equivalence)
    }

    delta <- abs(diff(equivalence)) / 2
    null <- min(equivalence) + delta

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
