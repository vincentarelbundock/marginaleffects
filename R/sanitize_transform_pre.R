sanitize_transform_pre <- function(transform_pre) {
    checkmate::assert(
        checkmate::check_function(transform_pre),
        checkmate::check_choice(transform_pre,
                                choices = c("difference",
                                            "ratio",
                                            "lnratio",
                                            "ratioavg",
                                            "lnratioavg",
                                            "differenceavg"))
    )

    if (is.function(transform_pre)) {
        out <- list("label" = "%s, %s", "function" = transform_pre)
        return(out)
    }

    fun <- list(
        "difference" = NULL, # return NULL so we can detect the default and warn when not arg not supported (e.g., Bayesian)
        "differenceavg" = function(hi, lo) mean(hi) - mean(lo),
        "ratio" = function(hi, lo) hi / lo,
        "lnratio" = function(hi, lo) log(hi / lo),
        "ratioavg" = function(hi, lo) mean(hi) / mean(lo),
        "lnratioavg" = function(hi, lo) log(mean(hi) / mean(lo))
    )[[transform_pre]]

    lab <- list(
        "difference" = "%s - %s", # return NULL so we can detect the default and warn when not arg not supported (e.g., Bayesian)
        "differenceavg" = "mean(%s) - mean(%s)",
        "ratio" = "%s / %s",
        "lnratio" = "ln(%s / %s)",
        "ratioavg" = "mean(%s) / mean(%s)",
        "lnratioavg" = "ln(mean(%s) / mean(%s))"
    )[[transform_pre]]

    out <- list("label" = lab, "function" = fun)
    return(out)
}

