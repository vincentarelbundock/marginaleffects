source("helpers.R")
using("marginaleffects")
library("MASS")
# library("brms")

dat <- transform(mtcars, gear = factor(gear))
mod <- lm(hp ~ mpg * qsec * am, data = dat)
mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
# mod <- brm(hp ~ mpg * qsec * am, data = dat, backend = "cmdstanr")


factory <- function(var_by = NULL,
                    var_label = c("rowid", var_by),
                    fun_hi = function(z) z,
                    fun_lo = function(z) z[1],
                    fun_comparison = function(hi, lo) hi - lo,
                    fun_label = function(z) sprintf("%s - %s", z, z[1])) {

    checkmate::assert_character(var_label, min.len = 1)

    fun <- function(x, newdata, draws) {
        if (!is.null(draws)) {
            insight::format_error("The `var_by` argument is not supported for models with draws.")
        }

        extra <- c(var_by, var_label)
        if ("rowid" %in% extra) extra <- unique(c("rowid", extra))

        flag_extra_columns <- any(!extra %in% colnames(x)) && !is.null(newdata)
        if (flag_extra_columns) {
            x <- merge(x, newdata)
            data.table::setDT(x)
        }

        var_label <- setdiff(var_label, var_by)
        tmp <- x[, ..var_label]
        for (col in colnames(tmp)) {
            tmp[, (col) := sprintf("%s[%s]", col, tmp[[col]])]
        }
        tmp <- apply(tmp, 1, paste, collapse = " & ")
        x[, term := tmp]

        if (is.null(var_by)) {
            out <- x[, list(term = fun_label(term), estimate = fun_comparison(fun_hi(estimate), fun_lo(estimate)))]
        } else {
            out <- x[, list(term = fun_label(term), estimate = fun_comparison(fun_hi(estimate), fun_lo(estimate))), by = var_by]
        }

        return(out)
    }

    return(fun)
}


f <- factory(
    var_by = "mpg",
    var_label = "group"
)

avg_predictions(mod,
    by = "mpg",
    hypothesis = f,
    newdata = datagrid(mpg = range, qsec = fivenum)
)

f <- factory(
    var_by = "mpg",
    var_label = "group",
    fun_hi = function(z) z,
    fun_lo = function(z) z[1],
    fun_comparison = function(hi, lo) hi / lo,
    fun_label = function(z) sprintf("%s / %s", z, z[1]))

predictions(mod,
    by = "mpg",
    hypothesis = f,
    newdata = datagrid(mpg = range, qsec = fivenum)
)

