source("helpers.R")
using("marginaleffects")
library("MASS")
# library("brms")

dat <- transform(mtcars, gear = factor(gear))
mod <- lm(hp ~ mpg * qsec * am, data = dat)
mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
# mod <- brm(hp ~ mpg * qsec * am, data = dat, backend = "cmdstanr")

# Q
# pkgload::load_all()

factory <- function(var_by = NULL,
                    var_label = c("rowid", var_by),
                    fun_estimate = function(z) z - z[1],
                    fun_label = function(z) sprintf("[%s] - [%s]", z, z[1])) {

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

        tmp <- apply(x[, ..var_label], 1, paste, collapse = "; ")
        x[, term := tmp]

        if (is.null(var_by)) {
            out <- x[, list(term = fun_label(term), estimate = fun_estimate(estimate))]
        } else {
            out <- x[, list(term = fun_label(term), estimate = fun_estimate(estimate)), by = var_by]
        }

        idx <- out$estimate != 0
        out <- out[idx]

        return(out)
    }

    return(fun)
}


f <- factory(
    var_by = c("mpg"),
    var_label = c("mpg", "group"),
    fun_estimate = function(z) z - data.table::shift(z),
    fun_label = function(z) sprintf("[%s] - [%s]", z, data.table::shift(z)))

predictions(mod,
    by = "mpg",
    hypothesis = f,
    newdata = datagrid(mpg = range, qsec = fivenum)
)

f <- factory(
    var_by = "mpg",
    var_label = c("rowid", "mpg"),
    fun_estimate = function(z) z - z[1],
    fun_label = function(z) sprintf("[%s] - [%s]", z, z[1]))

predictions(mod,
    hypothesis = f,
    newdata = datagrid(mpg = range, qsec = fivenum)
)

