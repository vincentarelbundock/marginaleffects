hypothesis_apply <- function(x,
                             labels,
                             hypothesis_by = NULL,
                             comparison_fun,
                             label_fun,
                             index_fun,
                             by_fun,
                             newdata) {
    insight::check_if_installed("collapse")
    draws <- attr(x, "posterior_draws")
    args <- list(matrix(x$estimate), FUN = index_fun, sort = FALSE)

    if (is.null(hypothesis_by)) {
        applyfun <- collapse::dapply
    } else {
        if (hypothesis_by %in% colnames(x)) {
            byval <- x[[hypothesis_by]]
        } else if (hypothesis_by %in% colnames(newdata)) {
            byval <- newdata[[hypothesis_by]]
        } else {
            msg <- sprintf("`%s` is not available in `newdata`.", hypothesis_by)
            stop(msg, call. = FALSE)
        }
        applyfun <- collapse::BY
        args[["g"]] <- byval
    }

    index <- drop(do.call(applyfun, args))

    args[["FUN"]] <- comparison_fun
    estimates <- do.call(applyfun, args)
    # estimates <- estimates[index, , drop = FALSE]

    if (!is.null(draws)) {
        args[[1]] <- draws
        draws <- do.call(applyfun, args)
        # draws <- draws[index, , drop = FALSE]
    }

    if (!is.null(labels)) {
        args[["FUN"]] <- label_fun
        args[[1]] <- matrix(labels)
        labels <- do.call(applyfun, args)
        # labels <- labels[index, , drop = FALSE]
    }

    if (!is.null(hypothesis_by)) {
        args[["FUN"]] <- by_fun
        args[[1]] <- byval
        byval <- do.call(applyfun, args)
        # byval <- byval[index]
    }

    out <- data.frame(
        hypothesis = drop(labels),
        estimate = drop(estimates)
    )
    out[[hypothesis_by]] <- byval

    attr(out, "posterior_draws") <- draws

    return(out)
}
