# TODO: multiple terms comparisons



source("helpers.R")
using("marginaleffects")
library("MASS")
# library("brms")

dat <- transform(mtcars, gear = factor(gear))
mod <- lm(hp ~ mpg * qsec * am + factor(cyl), data = dat)
# mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
# mod <- brm(hp ~ mpg * qsec * am, data = dat, backend = "cmdstanr")


hypothesis_helper <- function(
    by = c("term", "group", "contrast"),
    hypothesis = "reference",
    label = NULL,
    label_columns = NULL) {

    checkmate::assert_character(by, null.ok = TRUE)
    checkmate::assert_function(label, null.ok = TRUE)
    checkmate::assert_character(label_columns, null.ok = TRUE)
    checkmate::assert(
        checkmate::check_function(hypothesis),
        checkmate::check_choice(hypothesis, choices = c("reference", "sequential"))
    )

    if (is.null(label)) label <- function(estimate) "custom"

    if (identical(hypothesis, "reference")) {
        hypothesis <- function(x) (x - x[1])[2:length(x)]
        label = function(x) sprintf("(%s) - (%s)", x, x[1])[2:length(x)]
    } else if (identical(hypothesis, "sequential")) {
        hypothesis <- function(x) (x - data.table::shift(x))[2:length(x)]
        label = function(x) sprintf("(%s) - (%s)", x, data.table::shift(x))[2:length(x)]
    }

    fun <- function(x) {

        x <- data.table::copy(x)
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
        if (is.null(label_columns)) {
            label_columns <- c("group", "term", "rowid", attr(x, "variables_datagrid"), attr(x, "by"))
        }
        label_columns <- setdiff(label_columns, setdiff(by, "rowid"))
        label_columns <- intersect(label_columns, colnames(x))
        if (length(label_columns) == 0) label_columns <- "rowid"

        tmp <- x[, ..label_columns]
        for (col in colnames(tmp)) {
            tmp[, (col) := sprintf("%s[%s]", col, tmp[[col]])]
        }
        tmp <- apply(tmp, 1, paste, collapse = ", ")
        x[, marginaleffects_internal_label := tmp]

        if (is.null(by)) {
            out <- x[, list(
                hypothesis = label(marginaleffects_internal_label),
                estimate = hypothesis(estimate))]
        } else {
            out <- x[, list(
                hypothesis = label(marginaleffects_internal_label),
                estimate = hypothesis(estimate)),
            by = by]
        }

        attr(out, "hypothesis_function_by") <- by
        return(out)
    }

    return(fun)
}

# fun <- hypothesis_helper(by = c("term", "contrast", "am"), hypothesis = "sequential")
# comparisons(mod, hypothesis = fun) |> dplyr::arrange(term) |>  print(nrows = 100)

nd <- datagrid(am = 0:1, qsec = fivenum, model = mod)

predictions(mod, newdata = nd)

hyp <- hypothesis_helper()
pkgload::load_all()
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(by = "am")
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(by = "am", hypothesis = "sequential")
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(hypothesis = "sequential")
predictions(mod, by = c("am", "cyl"))
predictions(mod, newdata = nd, hypothesis = hyp)

hyp <- hypothesis_helper(hypothesis = "reference", by = "am")
predictions(mod, by = c("am", "cyl"), hypothesis = hyp)

hyp <- hypothesis_helper(
    by = "am",
    hypothesis = \(x) (x / x[1])[2:length(x)],
    label = \(x) sprintf("(%s) / (%s)", x, x[1])[2:length(x)],
    label_columns = c("rowid", "mpg")
)
predictions(mod, 
    newdata = datagrid(am = unique, qsec = fivenum, mpg = range), 
    hypothesis = hyp)

