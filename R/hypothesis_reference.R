hypothesis_reference <- function(estimates,
                                 labels,
                                 hypothesis_by = NULL,
                                 comparison = "difference",
                                 reverse = FALSE,
                                 newdata = newdata) {
    insight::check_if_installed("collapse")

    # reverse=FALSE: x - reference
    # reverse=TRUE: reference - x
    index_fun <- function(x) c(FALSE, rep(TRUE, length(x) - 1))

    by_fun <- function(x) x

    if (isTRUE(reverse)) {
        if (comparison == "difference") {
            comparison_fun <- function(x) x[1] - x
            label_fun <- function(x) sprintf("%s - %s", x[1], x)
        } else if (comparison == "ratio") {
            comparison_fun <- function(x) (x[1] / x)
            label_fun <- function(x) sprintf("%s / %s", x[1], x)
        }
    } else {
        if (comparison == "difference") {
            comparison_fun <- function(x) (x - x[1])
            label_fun <- function(x) sprintf("%s - %s", x, x[1])
        } else if (comparison == "ratio") {
            comparison_fun <- function(x) (x / x[1])
            label_fun <- function(x) sprintf("%s / %s", x, x[1])
        }
    }

    out <- hypothesis_apply(estimates,
        labels = labels,
        hypothesis_by = hypothesis_by,
        comparison_fun = comparison_fun,
        label_fun = label_fun,
        index_fun = index_fun,
        by_fun = by_fun,
        newdata = newdata)

    return(out)
}
