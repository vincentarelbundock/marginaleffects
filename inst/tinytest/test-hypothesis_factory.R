# TODO: multiple terms comparisons



source("helpers.R")
using("marginaleffects")
library("MASS")
# library("brms")

dat <- transform(mtcars, gear = factor(gear))
mod <- lm(hp ~ mpg * qsec * am, data = dat)
mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
# mod <- brm(hp ~ mpg * qsec * am, data = dat, backend = "cmdstanr")


factory <- function(by = c("term", "group", "contrast"),
                    FUN = function(x) x - x[1],
                    label = function(x) sprintf("%s - %s", x, x[1]),
                    label_row = "rowid") {

    checkmate::assert_character(by, null.ok = TRUE)

    fun <- function(x) {

        estimate <- x$estimate

        # automatic by argument
        if (is.null(by)) {
            by <- grep("^term$|^contrast|^group$", colnames(x), value = TRUE)
            if (length(by) == 0) by <- NULL
        } else {
            bad <- setdiff(by, c(colnames(x), "term", "group", "contrast", "rowid"))
            if (length(bad) > 0) {
                msg <- sprintf("Missing column(s): %s", paste(bad, collapse = ", "))
                insight::format_error(msg)
            }
            by <- intersect(by, colnames(x))
            if (length(by) == 0) by <- NULL
        }

        # row labels
        if (!"rowid" %in% colnames(x)) x[, "rowid" := seq_len(.N)]
        label_row <- setdiff(label_row, setdiff(by, "rowid"))
        label_row <- intersect(label_row, colnames(x))
        if (length(label_row) == 0) label_row <- "rowid"

        tmp <- x[, ..label_row]
        for (col in colnames(tmp)) {
            tmp[, (col) := sprintf("%s[%s]", col, tmp[[col]])]
        }
        tmp <- apply(tmp, 1, paste, collapse = ",")
        x[, marginaleffects_internal_label := tmp]

        if (is.null(by)) {
            out <- x[, list(
                hypothesis = label(marginaleffects_internal_label),
                estimate = FUN(estimate))]
        } else {
            out <- x[, list(
                hypothesis = label(marginaleffects_internal_label),
                estimate = FUN(estimate)),
            by = by]
        }

        attr(out, "hypothesis_function_by") <- by
        return(out)
    }

    return(fun)
}


fun <- factory(label_row = c("rowid", "group", "mpg"))

comparisons(mod, hypothesis = factory())

fun <- factory(label_row = c("rowid", "group", "mpg"))
predictions(mod, newdata = datagrid(mpg = range, qsec = fivenum))

fun <- factory(
    by = "mpg",
    FUN = \(x) x / x[1],
    label = \(x) sprintf("%s / %s", x, x[1]),
    label_row = "group"
)

fun = factory(by = c("group", "mpg"))
p = predictions(mod,
    hypothesis = fun,
    newdata = datagrid(mpg = range, qsec = fivenum))
p

