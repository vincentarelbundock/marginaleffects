get_hypothesis <- function(x, hypothesis, column) {

    if (is.null(hypothesis)) {
        return(x)
    }

    # must be checked here when we know how many rows the output has
    if (isTRUE(hypothesis %in% c("pairwise", "reference"))) {
        if (nrow(x) > 25) {
            msg <- format_msg(
            'The "pairwise" option of the `hypothesis` argument is not supported for
            `marginaleffects` commands which generate more than 25 rows of results. Use the
            `newdata`, `by`, and/or `variables` arguments to compute a smaller set of
            results on which to conduct hypothesis tests.')
            stop(msg, call. = FALSE)
        }
    }

    if (isTRUE(checkmate::check_numeric(hypothesis)) && isTRUE(checkmate::check_atomic_vector(hypothesis))) {
        if (length(hypothesis) != nrow(x)) {
            msg <- sprintf(
            "The `hypothesis` vector must be of length %s.", nrow(x))
            stop(msg, call. = FALSE)
        }
        out <- data.table(
            term = "custom",
            tmp = as.vector(x[[column]] %*% hypothesis))
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    if (isTRUE(checkmate::check_matrix(hypothesis))) {
        if (nrow(hypothesis) != nrow(x)) {
            msg <- sprintf(
            "The `hypothesis` matrix must be have %s rows.", nrow(x))
            stop(msg, call. = FALSE)
        }
        out <- data.table(
            term = "custom",
            tmp = as.vector(x[[column]] %*% hypothesis))
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    if (isTRUE(hypothesis == "reference")) {
        lab <- NULL
        mat <- list()
        for (j in 2:nrow(x)) {
            tmp <- matrix(0, nrow = nrow(x), ncol = 1)
            tmp[1, ] <- -1
            tmp[j, ] <- 1
            mat <- c(mat, list(tmp))
            lab <- c(lab, sprintf("Row %s - Row 1", j))
        }
        lc <- do.call("cbind", mat)
        out <- data.table(
            term = lab,
            tmp = as.vector(x[[column]] %*% lc))
        out <- out[out$term != "1 - 1", , drop = FALSE]
        setnames(out, old = "tmp", new = column)
        return(out)
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
        lc <- do.call("cbind", mat)
        out <- data.table(
            term = lab,
            tmp = as.vector(x[[column]] %*% lc))
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    # we assume this is a string formula
    if (is.character(hypothesis)) {
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
    }

    msg <- 
    "`hypothesis` is broken. Please report this bug:
    https://github.com/vincentarelbundock/marginaleffects/issues."
    stop(msg, call. = FALSE)
}


