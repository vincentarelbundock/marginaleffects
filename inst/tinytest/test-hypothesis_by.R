source("helpers.R")
using("marginaleffects")
requiet("MASS")

dat <- transform(mtcars, gear = factor(gear))
mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
mod <- lm(hp ~ mpg * qsec * am, data = dat)

# library(collapse)
# k = matrix(rnorm(50), nrow = 10)
# g = factor(rep(1:2, each = 5))
# sta <- BY(k, g = g, FUN = function(x) x[1])
# out <- TRA(k, g = g, FUN = "-", STATS = sta)


# Q
# pkgload::load_all()

fun_reference <- function(x, newdata = NULL, hypothesis_by = NULL) {
    checkmate::assert_character(hypothesis_by, null.ok = TRUE)
    if (any(!hypothesis_by %in% colnames(x)) && !is.null(newdata)) {
        x <- merge(x, newdata)
    }
    data.table::setDT(x)
    x[, term := get_hypothesis_row_labels(x, by = NULL)]
    x[, term := as.character(term)]
    if (is.null(hypothesis_by)) {
        x[, term := sprintf("%s - %s", term, term[1])]
        x[, estimate := estimate - estimate[1]]
    } else {
        x[, term := sprintf("%s - %s", term, .SD$term[1]), by = hypothesis_by]
        x[, estimate := estimate - estimate[1], by = hypothesis_by]
    }
    x <- x[estimate != 0]
    return(x)
}

predictions(mod,
    hypothesis_by = "mpg",
    hypothesis = fun_reference,
    newdata = datagrid(mpg = range, qsec = fivenum)
)

