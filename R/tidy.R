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

    mfx <- mfx[, colnames(mfx) %in% c("group", "term", "dydx", "std.error")]
    colnames(mfx)[match("dydx", colnames(mfx))] <- "estimate"

    if ("group" %in% colnames(mfx)) { 
        if ("std.error" %in% colnames(mfx)) {
            out <- mfx %>%
                poorman::group_by(group, term) %>%
                poorman::summarize(across(c("estimate", "std.error"), mean))
        } else {
            out <- mfx %>%
                poorman::group_by(group, term) %>%
                poorman::summarize(estimate = mean(estimate))
        }
    } else {
        if ("std.error" %in% colnames(mfx)) {
            out <- mfx %>%
                poorman::group_by(term) %>%
                poorman::summarize(across(c("estimate", "std.error"), mean))
        } else {
            out <- mfx %>%
                poorman::group_by(term) %>%
                poorman::summarize(estimate = mean(estimate))
        }
    }

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


#' @export
glance.marginaleffects <- function(mfx, ...) {
    out <- attr(mfx, "glance")
    return(out)
}
