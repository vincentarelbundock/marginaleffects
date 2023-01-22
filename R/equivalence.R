equivalence <- function(x, region = NULL, df = Inf, ...) {

    if (is.null(region)) {
        return(x)
    }

    checkmate::assert_numeric(region, min.len = 1, max.len = 2)
    if (length(region) == 1) {
        region <- c(region, region)
    }

    delta <- abs(diff(region)) / 2
    null <- min(region) + delta
    # x$region.null <- null
    # x$region.low <- max(region)
    # x$region.high <- min(region)

    # definitions from `emmeans`, with a different user interface based on symmetric "region"
    x$statistic.sup <- (x$estimate - null - delta) / x$std.error
    x$statistic.inf <- (x$estimate - null + delta) / x$std.error

    if (is.infinite(df)) {
        x$p.value.sup <- stats::pnorm(x$statistic.sup, lower.tail = TRUE)
        x$p.value.inf <- stats::pnorm(x$statistic.inf, lower.tail = FALSE)
    } else {
        x$p.value.sup <- stats::pt(x$statistic.sup, lower.tail = TRUE, df = x[["df"]])
        x$p.value.inf <- stats::pt(x$statistic.inf, lower.tail = FALSE, df = x[["df"]])
    }
    x$p.value.equ <- pmax(x$p.value.sup, x$p.value.inf)

    return(x)
}