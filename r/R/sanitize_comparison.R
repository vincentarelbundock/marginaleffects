wmean <- function(x, w) {
    stats::weighted.mean(x, w)
}

comparison_function_dict <- list(
    # default = difference between predictions
    "difference" = function(hi, lo) hi - lo,
    "differenceavg" = function(hi, lo) mean(hi - lo),
    "differenceavgwts" = function(hi, lo, w) wmean(hi - lo, w),

    # slopes and elasticities
    "dydx" = function(hi, lo, eps) (hi - lo) / eps,
    "eyex" = function(hi, lo, eps, y, x) (hi - lo) / eps * (x / y),
    "eydx" = function(hi, lo, eps, y, x) ((hi - lo) / eps) / y,
    "dyex" = function(hi, lo, eps, x) ((hi - lo) / eps) * x,

    # average slopes and elasticities
    "dydxavg" = function(hi, lo, eps) mean((hi - lo) / eps),
    "eyexavg" = function(hi, lo, eps, y, x) mean((hi - lo) / eps * (x / y)),
    "eydxavg" = function(hi, lo, eps, y, x) mean(((hi - lo) / eps) / y),
    "dyexavg" = function(hi, lo, eps, x) mean(((hi - lo) / eps) * x),
    "dydxavgwts" = function(hi, lo, eps, w) wmean((hi - lo) / eps, w),
    "eyexavgwts" = function(hi, lo, eps, y, x, w) wmean((hi - lo) / eps * (x / y), w),
    "eydxavgwts" = function(hi, lo, eps, y, x, w) wmean(((hi - lo) / eps) / y, w),
    "dyexavgwts" = function(hi, lo, eps, x, w) wmean(((hi - lo) / eps) * x, w),

    # ratios
    "ratio" = function(hi, lo) hi / lo,
    "ratioavg" = function(hi, lo) mean(hi) / mean(lo),
    "ratioavgwts" = function(hi, lo, w) wmean(hi, w) / wmean(lo, w),

    "lnratio" = function(hi, lo) log(hi / lo),
    "lnratioavg" = function(hi, lo) log(mean(hi) / mean(lo)),
    "lnratioavgwts" = function(hi, lo, w) log(wmean(hi, w) / wmean(lo, w)),

    "lnor" = function(hi, lo) log((hi / (1 - hi)) / (lo / (1 - lo))),
    "lnoravg" = function(hi, lo) log((mean(hi) / (1 - mean(hi))) / (mean(lo) / (1 - mean(lo)))),
    "lnoravgwts" = function(hi, lo, w)
        log(
            (wmean(hi, w) / (1 - wmean(hi, w))) / (wmean(lo, w) / (1 - wmean(lo, w)))
        ),

    # others
    "lift" = function(hi, lo) (hi - lo) / lo,
    "liftavg" = function(hi, lo) (mean(hi - lo)) / mean(lo),
    "liftavgwts" = function(hi, lo, w) (wmean(hi - lo, w)) / wmean(lo, w),

    "expdydx" = function(hi, lo, eps) ((exp(hi) - exp(lo)) / exp(eps)) / eps,
    "expdydxavg" = function(hi, lo, eps) mean(((exp(hi) - exp(lo)) / exp(eps)) / eps),
    "expdydxavgwts" = function(hi, lo, eps, w) wmean(((exp(hi) - exp(lo)) / exp(eps)) / eps, w)
)

comparison_label_dict <- list(
    "difference" = "%s - %s",
    "differenceavg" = "%s - %s",
    "differenceavgwts" = "%s - %s",
    "dydx" = "dY/dX",
    "eyex" = "eY/eX",
    "eydx" = "eY/dX",
    "dyex" = "dY/eX",
    "dydxavg" = "dY/dX",
    "eyexavg" = "eY/eX",
    "eydxavg" = "eY/dX",
    "dyexavg" = "dY/eX",
    "dydxavg" = "dY/dX",
    "eyexavg" = "eY/eX",
    "eydxavg" = "eY/dX",
    "dyexavg" = "dY/eX",
    "dydxavgwts" = "dY/dX",
    "eyexavgwts" = "eY/eX",
    "eydxavgwts" = "eY/dX",
    "dyexavgwts" = "dY/eX",
    "ratio" = "%s / %s",
    "ratioavg" = "mean(%s) / mean(%s)",
    "ratioavgwts" = "mean(%s) / mean(%s)",
    "lnratio" = "ln(%s / %s)",
    "lnratioavg" = "ln(mean(%s) / mean(%s))",
    "lnratioavgwts" = "ln(mean(%s) / mean(%s))",
    "lnor" = "ln(odds(%s) / odds(%s))",
    "lnoravg" = "ln(odds(%s) / odds(%s))",
    "lnoravgwts" = "ln(odds(%s) / odds(%s))",

    # Keep %s placeholders so categorical contrasts retain lo/hi labels.
    "lift" = "lift(%s, %s)",
    "liftavg" = "lift(%s, %s)",
    "liftavgwts" = "lift(%s, %s)",
    "expdydx" = "exp(dY/dX)"
)

sanity_comparison <- function(comparison) {
    # wts versions are used internally but not available directly to users
    valid <- names(comparison_function_dict)
    valid <- valid[!grepl("wts$", valid)]
    checkmate::assert(
        checkmate::check_choice(comparison, choices = valid),
        checkmate::check_function(comparison)
    )
}


sanitize_transform <- function(x) {
    good <- c("exp", "ln")
    # issue #1115: sanitize_transform() wraps `transform` into a named list, so the assertion may fail when using `inferences()`
    if (isTRUE(checkmate::check_list(x, names = "named"))) {
        checkmate::assert(
            checkmate::check_choice(x[[1]], choices = good, null.ok = TRUE),
            checkmate::check_function(x[[1]])
        )
        x <- x[[1]]
    } else {
        checkmate::assert(
            checkmate::check_choice(x, choices = good, null.ok = TRUE),
            checkmate::check_function(x)
        )
    }

    if (is.null(x)) {
        return(x)
    }

    if (is.function(x)) {
        out <- list(x)
        names(out) <- deparse(substitute(x))
    } else if (x == "exp") {
        out <- list("exp" = exp)
    } else if (x == "ln") {
        out <- list("ln" = log)
    }

    return(out)
}
