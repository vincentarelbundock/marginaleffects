source("helpers.R")
using("marginaleffects")
library("MASS")
# library("brms")

dat <- transform(mtcars, gear = factor(gear))
mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
mod <- lm(hp ~ mpg * qsec * am, data = dat)
# mod <- brm(hp ~ mpg * qsec * am, data = dat, backend = "cmdstanr")

# Q
# pkgload::load_all()

factory <- function(x,
                    newdata = NULL,
                    hypothesis_by = NULL,
                    lab_columns = c("rowid", hypothesis_by),
                    fun_estimate = function(z) z - z[1],
                    fun_lab = function(z) sprintf("[%s] - [%s]", z, z[1]),
                    draws = NULL) {

    checkmate::assert_character(lab_columns, min.len = 1)

    if (!is.null(draws)) {
        insight::format_error("The `hypothesis_by` argument is not supported for models with draws.")
    }

    extra <- c(hypothesis_by, lab_columns)
    if ("rowid" %in% extra) extra <- unique(c("rowid", extra))

    flag_extra_columns <- any(!extra %in% colnames(x)) && !is.null(newdata)
    if (flag_extra_columns) {
        x <- merge(x, newdata)
        data.table::setDT(x)
    }

    tmp <- apply(x[, ..extra], 1, paste, collapse = "; ")
    x[, term := tmp]

    if (is.null(hypothesis_by)) {
        out <- x[, list(term = fun_lab(term), estimate = fun_estimate(estimate))]
    } else {
        out <- x[, list(term = fun_lab(term), estimate = fun_estimate(estimate)), by = hypothesis_by]
    }

    idx <- out$estimate != 0
    out <- out[idx]

    return(out)
}


predictions(mod,
    hypothesis = factory,
    hypothesis_by = "mpg",
    newdata = datagrid(mpg = range, qsec = fivenum)

)
predictions(mod,

    hypothesis = hyp_by,
    hypothesis_by = "mpg",
    fun_estimate = function(z) z - data.table::shift(z),
    fun_lab = function(z) sprintf("%s vs. %s", z, data.table::shift(z)),
    newdata = datagrid(mpg = range, qsec = fivenum)
)

