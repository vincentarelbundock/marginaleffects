equivalence <- function(x, delta, null, side, df) {
    checkmate::assert_choice(side, choices = c("equivalence", "noninferiority", "nonsuperiority"))
    checkmate::assert_number(delta, lower = 1e-5)

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