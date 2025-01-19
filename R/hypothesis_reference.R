hypothesis_reference <- function(estimates, labels, draws = NULL, by = NULL, reverse = FALSE) {
    insight::check_if_installed("collapse")

    if (isTRUE(reverse)) {
        index_fun <- function(x) c(rep(TRUE, length(x) - 1), FALSE)
        if (comparison == "difference") {
            comparison_fun <- function(x) (x - x[length(x)])
            label_fun <- function(x) sprintf("(%s) - (%s)", x, x[length(x)])
        } else if (comparison == "ratio") {
            comparison_fun <- function(x) (x / x[length(x)])
            label_fun <- function(x) sprintf("(%s) / (%s)", x, x[length(x)])
        }
    } else {
        index_fun <- function(x) c(FALSE, rep(TRUE, length(x) - 1))
        if (comparison == "difference") {
            comparison_fun <- function(x) (x - x[1])
            label_fun <- function(x) sprintf("(%s) - (%s)", x, x[length(x)])
        } else if (comparison == "ratio") {
            comparison_fun <- function(x) (x / x[1])
            label_fun <- function(x) sprintf("(%s) / (%s)", x, x[length(x)])
        }
    }

    out <- hypothesis_apply(
        estimates = estimates,
        labels = labels,
        draws = draws,
        by = by,
        comparison_fun = comparison_fun,
        label_fun = label_fun,
        index_fun = index_fun)

    return(out)
}
