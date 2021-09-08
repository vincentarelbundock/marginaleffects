#' @importFrom generics tidy
#' @export
generics::tidy

#' @export
tidy.marginaleffects <- function(mfx,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 aggregation_function = mean) {

    mfx <- mfx[, colnames(mfx) %in% c("group", "term", "dydx", "std.error")]
    colnames(mfx)[match("dydx", colnames(mfx))] <- "estimate"

    if ("group" %in% colnames(mfx)) { 
        if ("std.error" %in% colnames(mfx)) {
            f <- cbind(estimate, std.error) ~ group + term
        } else {
            f <- estimate ~ group + term
        }
    } else {
        if ("std.error" %in% colnames(mfx)) {
            f <- cbind(estimate, std.error) ~ term
        } else {
            f <- estimate ~ term
        }
    }
    out <- aggregate(f, data = mfx, FUN = aggregation_function)

    if ("std.error" %in% colnames(out)) {
        out$statistic <- out$estimate / out$std.error
        out$p.value <- 2 * (1 - pnorm(abs(out$statistic)))
        if (isTRUE(conf.int)) {
            alpha <- 1 - conf.level
            out$conf.low <- out$estimate + qnorm(alpha / 2) * out$std.error
            out$conf.high <- out$estimate - qnorm(alpha / 2) * out$std.error
        }
    }

    return(out)
}
