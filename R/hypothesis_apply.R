hypothesis_apply <- function(x, labels, hypothesis_by = NULL, comparison_fun, label_fun, index_fun) {
    draws <- attr(x, "posterior_draws")
    args <- list(matrix(x$estimate))
    if (is.null(hypothesis_by)) {
        applyfun <- function(x, ...) collapse::dapply(x, ...)
    } else {
        applyfun <- function(x, ...) collapse::dapply(x, ...)
        args[["g"]] <- hypothesis_by
    }

    args[["FUN"]] <- index_fun
    index <- drop(do.call(applyfun, args))

    args[["FUN"]] <- comparison_fun
    estimates <- do.call(applyfun, args)
    estimates <- estimates[index, , drop = FALSE]

    if (!is.null(draws)) {
        args[[1]] <- draws
        draws <- do.call(applyfun, args)
        draws <- draws[index, , drop = FALSE]
    }

    if (!is.null(labels)) {
        args[[1]] <- matrix(labels)
        args[["FUN"]] <- label_fun
        labels <- do.call(applyfun, args)
        labels <- labels[index, , drop = FALSE]
    }

    out <- data.frame(
        hypothesis = drop(labels),
        estimate = drop(estimates)
    )
    attr(out, "posterior_draws") <- draws

    return(out)
}
