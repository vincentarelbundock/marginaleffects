# TODO: multiple terms comparisons



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

    fun <- function(x, draws) {
        if (!is.null(draws)) {
            insight::format_error("The `var_by` argument is not supported for models with draws.")
        }

        if (is.null(var_by)) {
            var_by <- grep("^term$|^contrast|^group$", colnames(x), value = TRUE)
            if (length(var_by) == 0) var_by <- NULL
        }

        var_label <- setdiff(var_label, var_by)
        var_label <- intersect(var_label, colnames(x))
        if (length(var_label) == 0 && !"rowid" %in% colnames(x)) {
            x[, "rowid" := seq_len(.N)]
            var_label <- "rowid"
        }
        tmp <- x[, ..var_label]
        for (col in colnames(tmp)) {
            tmp[, (col) := sprintf("%s[%s]", col, tmp[[col]])]
        }
        tmp <- apply(tmp, 1, paste, collapse = " & ")
        x[, marginaleffects_internal_label := tmp]

        if (is.null(var_by)) {
            out <- x[, list(
                hypothesis = fun_label(marginaleffects_internal_label), 
                estimate = fun_comparison(fun_hi(estimate), fun_lo(estimate)))]
        } else {
            out <- x[, list(
                hypothesis = fun_label(marginaleffects_internal_label),
                estimate = fun_comparison(fun_hi(estimate), fun_lo(estimate))), 
            by = var_by]
        }

        return(out)
    }

    return(fun)
}
fun <- factory()
comparisons(mod, hypothesis = fun)

avg_predictions(mod,
    by = "mpg",
    hypothesis = fun,
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
