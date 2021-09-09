#' @importFrom generics tidy
#' @export
generics::tidy


#' @importFrom generics glance
#' @export
generics::glance


#' @export
tidy.marginaleffects <- function(mfx,
                                 conf.int = TRUE,
                                 conf.level = 0.95,
                                 agg_fun = mean) {

    # dydx averages
    dydx <- mfx[, colnames(mfx) %in% c("group", "term", "dydx", "std.error")]
    colnames(dydx)[match("dydx", colnames(dydx))] <- "estimate"

    if ("group" %in% colnames(dydx)) { 
        if ("std.error" %in% colnames(dydx)) {
            dydx <- dydx %>%
                poorman::group_by(group, term) %>%
                poorman::summarize(across(c("estimate", "std.error"), mean))
        } else {
            dydx <- dydx %>%
                poorman::group_by(group, term) %>%
                poorman::summarize(estimate = mean(estimate))
        }
    } else {
        if ("std.error" %in% colnames(dydx)) {
            dydx <- dydx %>%
                poorman::group_by(term) %>%
                poorman::summarize(across(c("estimate", "std.error"), mean))
        } else {
            dydx <- dydx %>%
                poorman::group_by(term) %>%
                poorman::summarize(estimate = mean(estimate))
        }
    }

    # dydx statistics (emmeans calculates those for us)
    if (!"statistic" %in% colnames(dydx)) dydx$statistic <- dydx$estimate / dydx$std.error
    if (!"p.value" %in% colnames(dydx)) dydx$p.value <- 2 * (1 - pnorm(abs(dydx$statistic)))

    # contrasts
    out <- poorman::bind_rows(dydx, attr(mfx, "contrasts"))

    # confidence intervals
    if ("std.error" %in% colnames(out)) {
        if (isTRUE(conf.int) && !"conf.low" %in% colnames(out)) {
            alpha <- 1 - conf.level
            out$conf.low <- out$estimate + qnorm(alpha / 2) * out$std.error
            out$conf.high <- out$estimate - qnorm(alpha / 2) * out$std.error
        }
    }

    # sort and subset columns
    cols <- c("group", "term", "contrast", "estimate", "std.error",
              "statistic", "p.value", "conf.low", "conf.high")
    out <- out[, intersect(cols, colnames(out)), drop = FALSE]
    return(out)
}


#' @export
glance.marginaleffects <- function(mfx, ...) {
    out <- attr(mfx, "glance")
    return(out)
}
