equivalence <- function(x, region = NULL, side = NULL, df = Inf, ...) {

    if (is.null(region) || is.null(side)) {
        return(x)
    }

    checkmate::assert_numeric(region, len = 2)
    checkmate::assert_choice(
        side,
        choices = c("equivalence", "noninferiority", "nonsuperiority"),
        null.ok = TRUE)


    delta <- abs(diff(region)) / 2
    null <- min(region) + delta

    # definitions from `emmeans`, with a different user interface based on symmetric "region"
    if (side == "nonsuperiority") {
        x$statistic <- (x$estimate - null - delta) / x$std.error
    } else if (side == "noninferiority") {
        x$statistic <- (x$estimate - null + delta) / x$std.error
    } else if (side == "equivalence") {
        x$statistic <- (abs(x$estimate - null) - delta) / x$std.error
    }

    if (is.infinite(df)) {
        pval <- stats::pnorm(x$statistic)
    } else {
        pval <- stats::pt(x$statistic, df = x[["df"]])
    }

    if (side %in% c("nonsuperiority", "equivalence")) {
        x[["p.value"]] <- pval
    } else if (side == "noninferiority") {
        x[["p.value"]] <- 1 - pval
    }

    x[["conf.low"]] <- x[["conf.high"]] <- NULL

    return(x)
}