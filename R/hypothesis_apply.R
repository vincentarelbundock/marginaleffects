hypothesis_apply <- function(estimates, labels, draws, by, comparison_fun, label_fun, index_fun) {
    args <- list(FUN = comparison_fun)
    if (is.null(by)) {
        applyfun <- collapse::dapply
    } else {
        applyfun <- collapse::BY
        args[["g"]] <- by
    }

    args[["x"]] <- matrix(estimates)
    args[["FUN"]] <- index_fun
    index <- drop(do.call(applyfun, args))

    args[["FUN"]] <- comparison_fun
    estimates <- do.call(applyfun, args)
    estimates <- estimates[index, , drop = FALSE]

    if (!is.null(draws)) {
        args[["x"]] <- draws
        draws <- do.call(applyfun, args)
        draws <- draws[index, , drop = FALSE]
    }

    args[["FUN"]] <- label_fun
    args[["x"]] <- matrix(labels)
    labels <- do.call(applyfun, args)
    labels <- labels[index, , drop = FALSE]

    out <- list("estimates" = drop(estimates), "labels" = drop(labels), "draws" = draws)
    return(out)
}
