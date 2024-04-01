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


Q
pkgload::load_all()

fun_reference <- function(x, newdata = NULL, hypothesis_by = NULL) {
    checkmate::assert_character(hypothesis_by, null.ok = TRUE)
    flag_merge <- any(!hypothesis_by %in% colnames(x)) && !is.null(newdata)
    if (flag_merge) {
        x <- merge(x, newdata)
    }
    data.table::setDT(x)
    x[, term := get_hypothesis_row_labels(x, newdata = newdata, by = NULL, hypothesis_by = hypothesis_by)]
    x[, term := as.character(term)]
    if (is.null(hypothesis_by)) {
        x[, term := sprintf("%s vs. %s", term, term[1])]
        x[, estimate := estimate - estimate[1]]
    } else {
        x[, term := sprintf("%s vs. %s", term, .SD$term[1]), by = hypothesis_by]
        x[, estimate := estimate - estimate[1], by = hypothesis_by]
    }
    x <- x[estimate != 0]

    # otherwise we get useless and MISLEADING columns from `newdata` 
    # ex: comparing two rows with different values of `qsec`, but the qsec column only 
    # gives one number because we only modified estimate column
    if (flag_merge) {
        idx <- c("term", "estimate", hypothesis_by)
        x <- x[, ..idx]
    }

    return(x)
}

predictions(mod,
    hypothesis_by = "mpg",
    hypothesis = fun_reference,
    newdata = datagrid(mpg = range, qsec = fivenum)
)

