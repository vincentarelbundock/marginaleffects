sanitize_contrast_function <- function(contrast_function) {
    checkmate::assert(
        checkmate::check_function(contrast_function),
        checkmate::check_choice(contrast_function,
                                choices = c("difference",
                                            "ratio",
                                            "lnratio",
                                            "ratioavg",
                                            "lnratioavg",
                                            "differenceavg"))
    )

    if (is.function(contrast_function)) {
        return(contrast_function)
    }

    out <- list(
        "difference" = NULL, # return NULL so we can detect the default and warn when not arg not supported (e.g., Bayesian)
        "differenceavg" = function(hi, lo) mean(hi) - mean(lo),
        "ratio" = function(hi, lo) hi / lo,
        "lnratio" = function(hi, lo) log(hi / lo),
        "ratioavg" = function(hi, lo) mean(hi) / mean(lo),
        "lnratioavg" = function(hi, lo) log(mean(hi) / mean(lo))
    )[[contrast_function]]

    return(out)
}

