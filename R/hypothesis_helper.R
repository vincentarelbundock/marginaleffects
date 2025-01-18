#' (EXPERIMENTAL) This experimental function will soon be deprecated. Please supply a formula or function to the `hypothesis` argument to conduct (group-wise) hypothesis tests.
#'
#' @param hypothesis String or Function. Compute a test statistic.
#' - String: "reference" or "sequential"
#' - Function: Accepts a single argument named `estimate` and returns a numeric vector.
#' @param by Character vector. Variable names which indicate subgroups in which the `hypothesis` function should be applied.
#' @param label Function. Accepts a vector of row labels and combines them to create hypothesis labels.
#' @param label_columns Character vector. Column names to use for hypothesis labels. Default is `c("group", "term", "rowid", attr(x, "variables_datagrid"), attr(x, "by"))`.
#' @param comparison String. "ratio" or "difference"
#' @param internal Logical. Raises a deprecation warning when FALSE.
#' @return `specify_hypothesis()` is a "function factory", which means that executing it will return a function suitable for use in the `hypothesis` argument of a `marginaleffects` function.
#' @noRd
specify_hypothesis <- function(
    hypothesis = "reference",
    comparison = "difference",
    label = NULL,
    label_columns = NULL,
    by = c("term", "group", "contrast"),
    internal = FALSE) {
    if (!isTRUE(internal)) {
        insight::format_warning("The `specify_hypothesis()` function was marked as experimental and  will be deprecate. Use the formula interface to the `hypothesis` argument to specify group-wise hypothesis tests.")
    }
    checkmate::assert_choice(comparison, c("ratio", "difference"))
    checkmate::assert_character(by, null.ok = TRUE)
    checkmate::assert_function(label, null.ok = TRUE)
    checkmate::assert_character(label_columns, null.ok = TRUE)
    checkmate::assert(
        checkmate::check_function(hypothesis),
        checkmate::check_choice(hypothesis, choices = c("reference", "sequential", "meandev", "poly"))
    )

    if (is.null(label)) label <- function(x) "custom"

    if (identical(hypothesis, "reference")) {
        if (comparison == "difference") {
            hypothesis <- function(x) (x - x[1])[2:length(x)]
            label <- function(x) sprintf("(%s) - (%s)", x, x[1])[2:length(x)]
        } else {
            hypothesis <- function(x) (x / x[1])[2:length(x)]
            label <- function(x) sprintf("(%s) / (%s)", x, x[1])[2:length(x)]
        }
    } else if (identical(hypothesis, "sequential")) {
        if (comparison == "difference") {
            hypothesis <- function(x) (x - data.table::shift(x))[2:length(x)]
            label <- function(x) sprintf("(%s) - (%s)", x, data.table::shift(x))[2:length(x)]
        } else {
            hypothesis <- function(x) (x / data.table::shift(x))[2:length(x)]
            label <- function(x) sprintf("(%s) / (%s)", x, data.table::shift(x))[2:length(x)]
        }
    } else if (identical(hypothesis, "meandev")) {
        hypothesis <- function(x) x - mean(x)
        label <- function(x) sprintf("(%s) - Avg", x)
    } else if (identical(hypothesis, "poly")) {
        hypothesis <- function(x) {
            w <- stats::contr.poly(length(x))[, 1:3]
            drop(crossprod(w, matrix(x)))
        }
        label <- function(x) c("linear", "quadratic", "cubic")
    }

    fun <- function(x) {
        x <- data.table::copy(x)
        estimate <- x$estimate

        # automatic by argument
        if (!is.null(by)) {
            bad <- setdiff(by, c(colnames(x), "term", "group", "contrast", "rowid"))
            if (length(bad) > 0) {
                msg <- sprintf("Missing column(s): %s", paste(bad, collapse = ", "))
                insight::format_error(msg)
            }
            by <- intersect(by, colnames(x))
            if (length(by) == 0) by <- NULL
        }

        if (!inherits(x, "data.table")) {
            data.table::setDT(x)
        }

        # row labels
        if (!"rowid" %in% colnames(x)) x[, "rowid" := seq_len(.N)]
        if (is.null(label_columns)) {
            # TODO check if we need rowid
            # label_columns <- c("group", "term", "rowid", attr(x, "variables_datagrid"), attr(x, "by"))
            label_columns <- c("group", "term", attr(x, "variables_datagrid"), attr(x, "by"))
        }
        label_columns <- setdiff(label_columns, setdiff(by, "rowid"))
        label_columns <- intersect(label_columns, colnames(x))
        if (length(label_columns) == 0) label_columns <- "rowid"

        tmp <- x[, ..label_columns]

        label_columns <- Filter(function(n) length(unique(tmp[[n]])) > 1, names(tmp))
        for (col in label_columns) {
            if (length(unique(tmp[[col]])) == 1) {
                tmp[, (col) := NULL]
            } else {
                # drop unambiguous single label
                if (length(label_columns) > 1) {
                    tmp[, (col) := sprintf("%s[%s]", col, tmp[[col]])]
                } else {
                    tmp[, (col) := sprintf("%s", tmp[[col]])]
                }
            }
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
