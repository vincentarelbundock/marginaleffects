hypothesis_matrix <- function(x, hypothesis) {
    if (isTRUE(checkmate::check_atomic_vector(hypothesis))) {
        hypothesis <- matrix(hypothesis, ncol = 1)
    }
    checkmate::assert_matrix(hypothesis, nrows = nrow(x))

    if (is.null(colnames(hypothesis))) {
        colnames(hypothesis) <- rep("custom", ncol(hypothesis))
    }

    # bayesian
    draws <- attr(x, "posterior_draws")
    if (!is.null(draws)) {
        draws <- t(as.matrix(hypothesis)) %*% draws
        out <- data.table(
            term = colnames(hypothesis),
            tmp = apply(draws, 1, stats::median)
        )
        setnames(out, old = "tmp", new = "estimate")

        # frequentist
    } else {
        out <- data.table(
            term = colnames(hypothesis),
            tmp = as.vector(x[["estimate"]] %*% hypothesis)
        )
        setnames(out, old = "tmp", new = "estimate")
    }

    keep <- out$term != "1 - 1"
    out <- out[keep, , drop = FALSE]
    if (!is.null(draws)) {
        attr(out, "posterior_draws") <- draws[keep, , drop = FALSE]
    }
    return(out)
}
