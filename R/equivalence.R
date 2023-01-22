equivalence <- function(x, equivalence = NULL, df = Inf, ...) {

    if (is.null(equivalence) || any(!c("estimate", "std.error") %in% colnames(x))) {
        return(x)
    }

    checkmate::assert_numeric(equivalence, min.len = 1, max.len = 2)
    if (length(equivalence) == 1) {
        equivalence <- c(equivalence, equivalence)
    }

    delta <- abs(diff(equivalence)) / 2
    null <- min(equivalence) + delta
    # x$equivalence.null <- null
    # x$equivalence.low <- max(equivalence)
    # x$equivalence.high <- min(equivalence)

    # definitions from `emmeans`, with a different user interface based on symmetric "equivalence"
    x$statistic.inf <- (x$estimate - null + delta) / x$std.error
    x$statistic.sup <- (x$estimate - null - delta) / x$std.error

    if (is.infinite(df)) {
        x$p.value.inf <- stats::pnorm(x$statistic.inf, lower.tail = FALSE)
        x$p.value.sup <- stats::pnorm(x$statistic.sup, lower.tail = TRUE)
    } else {
        x$p.value.inf <- stats::pt(x$statistic.inf, lower.tail = FALSE, df = x[["df"]])
        x$p.value.sup <- stats::pt(x$statistic.sup, lower.tail = TRUE, df = x[["df"]])
    }
    x$p.value.equ <- pmax(x$p.value.sup, x$p.value.inf)

    return(x)
}