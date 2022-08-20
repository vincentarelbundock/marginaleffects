wmean <- function(x, w) {
    stats::weighted.mean(x, w)
}

transform_pre_function_dict <- list(
    # default = difference between predictions
    "difference" = function(hi, lo) hi - lo,
    "differenceavg" = function(hi, lo) mean(hi) - mean(lo),
    "differenceavgwts" = function(hi, lo, w) wmean(hi, w) - wmean(lo, w),

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
    "ratioavg" = function(hi, lo) wmean(hi) / wmean(lo),

    "lnratio" = function(hi, lo) log(hi / lo),
    "lnratioavg" = function(hi, lo) log(mean(hi) / mean(lo)),
    "lnratioavg" = function(hi, lo) log(wmean(hi) / wmean(lo)),

    "lnor" = function(hi, lo) log((hi / (1 - hi)) / (lo / (1 - lo))),
    "lnoravg" = function(hi, lo) log((mean(hi) / (1 - mean(hi))) / (mean(lo) / (1 - mean(lo)))),
    "lnoravgwts" = function(hi, lo, w) log((wmean(hi, w) / (1 - wmean(hi, w))) / (wmean(lo, w) / (1 - wmean(lo, w)))),

    # others
    "expdydx" = function(hi, lo, eps) ((exp(hi) - exp(lo)) / exp(eps)) / eps,
    "expdydxavg" = function(hi, lo, eps) mean(((exp(hi) - exp(lo)) / exp(eps)) / eps),
    "expdydxavgwts" = function(hi, lo, eps, w) wmean(((exp(hi) - exp(lo)) / exp(eps)) / eps, w)
)

transform_pre_label_dict <- list(
    "difference" = "%s - %s",
    "differenceavg" = "mean(%s) - mean(%s)",
    "differenceavgwts" = "mean(%s) - mean(%s)",

    "dydx" = "dY/dX",
    "eyex" = "eY/eX",
    "eydx" = "eY/dX",
    "dyex" = "dY/eX",

    "dydxavg" = "mean(dY/dX)",
    "eyexavg" = "mean(eY/eX)",
    "eydxavg" = "mean(eY/dX)",
    "dyexavg" = "mean(dY/eX)",

    "dydxavg" = "mean(dY/dX)",
    "eyexavg" = "mean(eY/eX)",
    "eydxavg" = "mean(eY/dX)",
    "dyexavg" = "mean(dY/eX)",
    "dydxavgwts" = "mean(dY/dX)",
    "eyexavgwts" = "mean(eY/eX)",
    "eydxavgwts" = "mean(eY/dX)",
    "dyexavgwts" = "mean(dY/eX)",

    "ratio" = "%s / %s",
    "ratioavg" = "mean(%s) / mean(%s)",
    "ratioavgwts" = "mean(%s) / mean(%s)",

    "lnratio" = "ln(%s / %s)",
    "lnratioavg" = "ln(mean(%s) / mean(%s))",
    "lnratioavgwts" = "ln(mean(%s) / mean(%s))",

    "lnor" = "ln(odds(%s) / odds(%s))",
    "lnoravg" = "ln(odds(%s) / odds(%s))",
    "lnoravgwts" = "ln(odds(%s) / odds(%s))",

    "expdydx" = "exp(dY/dX)"
)

sanity_transform_pre <- function(transform_pre) {
    checkmate::assert(
        checkmate::check_choice(transform_pre,
                                choices = names(transform_pre_function_dict)),
        checkmate::check_function(transform_pre))
}
