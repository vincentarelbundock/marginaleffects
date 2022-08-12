get_hypothesis <- function(x, hypothesis, column) {

    if (is.null(hypothesis)) {
        return(x)
    }

    lincom <- NULL

    # must be checked here when we know how many rows the output has
    if (isTRUE(hypothesis %in% c("pairwise", "reference", "sequential"))) {
        if (nrow(x) > 25) {
            msg <- format_msg(
            'The "pairwise", "reference", and "sequential" options of the `hypothesis`
            argument is not supported for `marginaleffects` commands which generate more
            than 25 rows of results. Use the `newdata`, `by`, and/or `variables` arguments
            to compute a smaller set of results on which to conduct hypothesis tests.')
            stop(msg, call. = FALSE)
        }
    }

    if (isTRUE(checkmate::check_numeric(hypothesis)) &&
        isTRUE(checkmate::check_atomic_vector(hypothesis))) {
        if (length(hypothesis) != nrow(x)) {
            msg <- sprintf(
            "The `hypothesis` vector must be of length %s.", nrow(x))
            stop(msg, call. = FALSE)
        }
        lincom <- as.matrix(hypothesis)
        colnames(lincom) <- "custom"
    }

    if (isTRUE(checkmate::check_matrix(hypothesis))) {
        if (nrow(hypothesis) != nrow(x)) {
            msg <- sprintf(
            "The `hypothesis` matrix must be have %s rows.", nrow(x))
            stop(msg, call. = FALSE)
        }
        lincom <- hypothesis
        colnames(lincom) <- rep("custom", ncol(lincom))
    }

    if (isTRUE(hypothesis == "reference")) {
        lincom <- diag(nrow(x))
        lincom[1, ] <- -1
        colnames(lincom) <- sprintf("Row %s - Row 1", 1:ncol(lincom))
        lincom <- lincom[, 2:ncol(lincom)]
    }

    if (isTRUE(hypothesis == "sequential")) {
        lincom <- matrix(0, nrow = nrow(x), ncol = nrow(x) - 1)
        for (i in 1:ncol(lincom)) {
            lincom[i:(i + 1), i] <- c(-1, 1)
        }
        colnames(lincom) <- sprintf("Row %s - Row %s", (1:ncol(lincom)) + 1, 1:ncol(lincom))
    }

    if (isTRUE(hypothesis == "pairwise")) {
        lab <- NULL
        mat <- list()
        for (i in 1:nrow(x)) {
            for (j in 2:nrow(x)) {
                if (i < j) {
                    tmp <- matrix(0, nrow = nrow(x), ncol = 1)
                    tmp[i, ] <- -1
                    tmp[j, ] <- 1
                    mat <- c(mat, list(tmp))
                    lab <- c(lab, sprintf("Row %s - Row %s", j, i))
                }
            }
        }
        lincom <- do.call("cbind", mat)
        colnames(lincom) <- lab
    }

    # we assume this is a string formula
    if (is.character(hypothesis) && is.null(lincom)) {
        envir <- parent.frame()

        # row indices: `hypothesis` includes them, but `term` does not
        if (isTRUE(grepl("\\bb\\d+\\b", hypothesis)) && !any(grepl("\\bb\\d+\\b", x[["term"]]))) {
            lab <- hypothesis
            for (i in seq_len(nrow(x))) {
                tmp <- paste0("marginaleffects__", i)
                hypothesis <- gsub(paste0("b", i), tmp, hypothesis)
                assign(tmp, x[[column]][i], envir = envir)
            }

        # term names
        } else {
            if (!"term" %in% colnames(x) || anyDuplicated(x$term) > 0) {
                msg <- format_msg(
                'To use term names in a `hypothesis` string, the same function call without
                `hypothesis` argument must produce a `term` column with unique row identifiers.
                You can use `b1`, `b2`, etc. indices instead of term names in the `hypothesis`
                string Ex: "b1 + b2 = 0" Alternatively, you can use the `newdata`, `variables`,
                or `by` arguments:

                mod <- lm(mpg ~ am * vs + cyl, data = mtcars)
                comparisons(mod, newdata = "mean", hypothesis = "am = vs")
                comparisons(mod, variables = "am", by = "cyl", hypothesis = "pairwise")
                ')
                stop(msg, call. = FALSE)
            }

            for (i in seq_len(nrow(x))) {
                tmp <- x$term[i]
                assign(tmp, x[[column]][i], envir = envir)
            }
        }

        out <- eval(parse(text = hypothesis), envir = envir)
        out <- data.table(
            term = gsub("\\s+", "", attr(hypothesis, "label")),
            tmp = out)
        setnames(out, old = "tmp", new = column)
        return(out)

    } else if (isTRUE(checkmate::check_numeric(lincom))) {

        # bayesian
        draws <- attr(x, "posterior_draws")
        if (!is.null(draws)) {
            draws <- t(as.matrix(lincom)) %*% draws
            out <- data.table(
                term = lab,
                tmp = apply(draws, 1, stats::median))
            setnames(out, old = "tmp", new = column)
            idx <- out$term != "1 - 1"
            draws <- draws[idx, , drop = FALSE]
            out <- out[idx, , drop = FALSE]
            attr(out, "posterior_draws") <- draws

        # frequentist
        } else {

            out <- data.table(
                term = colnames(lincom),
                tmp = as.vector(x[[column]] %*% lincom))
            setnames(out, old = "tmp", new = column)
        }

        out <- out[out$term != "1 - 1", , drop = FALSE]
        return(out)
    }

    msg <- 
    "`hypothesis` is broken. Please report this bug:
    https://github.com/vincentarelbundock/marginaleffects/issues."
    stop(msg, call. = FALSE)
}


