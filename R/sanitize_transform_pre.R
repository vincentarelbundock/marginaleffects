transform_pre_function_dict <- list(
    # return NULL so we can detect the default and warn when not arg not supported (e.g., Bayesian)
    "difference" = function(hi, lo) hi - lo,
    "differenceavg" = function(hi, lo) mean(hi) - mean(lo),
    "ratio" = function(hi, lo) hi / lo,
    "lnratio" = function(hi, lo) log(hi / lo),
    "ratioavg" = function(hi, lo) mean(hi) / mean(lo),
    "lnratioavg" = function(hi, lo) log(mean(hi) / mean(lo)),
    "lnoravg" = function(hi, lo) {
        m_hi <- mean(hi);
        m_lo <- mean(lo);
        log((m_hi / (1 - m_hi)) / (m_lo / (1 - m_lo)))
    },
    "dydx" = function(hi, lo, eps) (hi - lo) / eps,
    "eyex" = function(hi, lo, or, eps, x) (hi - lo) / eps * (x / or),
    "eydx" = function(hi, lo, or, eps, x) ((hi - lo) / eps) / or,
    "dyex" = function(hi, lo, eps, x) ((hi - lo) / eps) * x,
    "dydxavg" = function(hi, lo, eps) mean((hi - lo) / eps),
    "expdydx" = function(hi, lo, eps) ((exp(hi) - exp(lo)) / exp(eps)) / eps)

transform_pre_label_dict <- list(
    "difference" = "%s - %s", # return NULL so we can detect the default and warn when not arg not supported (e.g., Bayesian)
    "differenceavg" = "mean(%s) - mean(%s)",
    "ratio" = "%s / %s",
    "lnratio" = "ln(%s / %s)",
    "ratioavg" = "mean(%s) / mean(%s)",
    "lnratioavg" = "ln(mean(%s) / mean(%s))",
    "lnoravg" = "ln(odds(%s) / odds(%s))",
    "dydx" = "dydx",
    "eyex" = "eyex",
    "eydx" = "eydx",
    "dyex" = "dyex",
    "dydxavg" = "dydxavg",
    "expdydx" = "expdydx")


sanity_transform_pre <- function(transform_pre) {
    checkmate::assert(
        checkmate::check_function(transform_pre),
        checkmate::check_choice(transform_pre,
                                choices = c("difference",
                                            "differenceavg",
                                            "ratio",
                                            "lnratio",
                                            "ratioavg",
                                            "lnratioavg",
                                            "lnoravg",
                                            "dydx",
                                            "eyex",
                                            "eydx",
                                            "dyex",
                                            "dydxavg",
                                            "expdydx")))
}
