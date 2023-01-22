# This function takes any input from the `hypothesis` argument, builds a lincom matrix and then multiplies it by the estimates
get_hypothesis <- function(x, hypothesis, column, by = NULL) {

    if (is.null(hypothesis)) {
        return(x)
    }

    lincom <- NULL

    # must be checked here when we know how many rows the output has
    if (isTRUE(hypothesis %in% c("pairwise", "reference", "sequential"))) {
        if (nrow(x) > 25) {
            msg <- 'The "pairwise", "reference", and "sequential" options of the `hypotheses` argument are not supported for `marginaleffects` commands which generate more than 25 rows of results. Use the `newdata`, `by`, and/or `variables` arguments to compute a smaller set of results on which to conduct hypothesis tests.'
            insight::format_error(msg)
        }
    }

    if (isTRUE(checkmate::check_numeric(hypothesis)) &&
        isTRUE(checkmate::check_atomic_vector(hypothesis))) {
        if (length(hypothesis) != nrow(x)) {
            msg <- sprintf(
            "The `hypotheses` vector must be of length %s.", nrow(x))
            stop(msg, call. = FALSE)
        }
        lincom <- as.matrix(hypothesis)
        colnames(lincom) <- "custom"
    }

    if (isTRUE(checkmate::check_matrix(hypothesis))) {
        if (nrow(hypothesis) != nrow(x)) {
            msg <- sprintf(
            "The `hypotheses` matrix must be have %s rows.", nrow(x))
            stop(msg, call. = FALSE)
        }
        lincom <- hypothesis
        if (is.null(colnames(lincom))) {
            colnames(lincom) <- rep("custom", ncol(lincom))
        }
    }

    if (isTRUE(hypothesis == "revreference")) {
        lincom <- -1 * diag(nrow(x))
        lincom[1, ] <- 1
        lab <- get_hypothesis_row_labels(x, by = by)
        if (length(lab) == 0 || anyDuplicated(lab) > 0) {
            lab <- sprintf("Row 1 - Row %s", seq_len(ncol(lincom)))
        } else {
            lab <- sprintf("%s - %s", lab[1], lab)
        }
        colnames(lincom) <- lab
        lincom <- lincom[, 2:ncol(lincom), drop = FALSE]
    }

    if (isTRUE(hypothesis == "reference")) {
        lincom <- diag(nrow(x))
        lincom[1, ] <- -1
        lab <- get_hypothesis_row_labels(x, by = by)
        if (length(lab) == 0 || anyDuplicated(lab) > 0) {
            lab <- sprintf("Row %s - Row 1", seq_len(ncol(lincom)))
        } else {
            lab <- sprintf("%s - %s", lab, lab[1])
        }
        colnames(lincom) <- lab
        lincom <- lincom[, 2:ncol(lincom), drop = FALSE]
    }

    if (isTRUE(hypothesis == "revsequential")) {
        lincom <- matrix(0, nrow = nrow(x), ncol = nrow(x) - 1)
        lab <- get_hypothesis_row_labels(x, by = by)
        if (length(lab) == 0 || anyDuplicated(lab) > 0) {
            lab <- sprintf("Row %s - Row %s", seq_len(ncol(lincom)), seq_len(ncol(lincom)) + 1)
        } else {
            lab <- sprintf("%s - %s", lab[seq_len(ncol(lincom))], lab[seq_len(ncol(lincom)) + 1])
        }
        for (i in seq_len(ncol(lincom))) {
            lincom[i:(i + 1), i] <- c(1, -1)
        }
        colnames(lincom) <- lab
    }

    if (isTRUE(hypothesis == "sequential")) {
        lincom <- matrix(0, nrow = nrow(x), ncol = nrow(x) - 1)
        lab <- get_hypothesis_row_labels(x, by = by)
        if (length(lab) == 0 || anyDuplicated(lab) > 0) {
            lab <- sprintf("Row %s - Row %s", seq_len(ncol(lincom)) + 1, seq_len(ncol(lincom)))
        } else {
            lab <- sprintf("%s - %s", lab[seq_len(ncol(lincom)) + 1], lab[seq_len(ncol(lincom))])
        }
        for (i in seq_len(ncol(lincom))) {
            lincom[i:(i + 1), i] <- c(-1, 1)
        }
        colnames(lincom) <- lab
    }

    # same order as `emmeans`
    # in simple contrasts, revpairwise is trivially -1*pairwise, but it is
    # useful to have this option for interactions
    if (isTRUE(hypothesis == "revpairwise")) {
        lab_row <- get_hypothesis_row_labels(x, by = by)
        lab_col <- NULL
        flag <- length(lab_row) == 0 || anyDuplicated(lab_row) > 0
        mat <- list()
        for (i in seq_len(nrow(x))) {
            for (j in 2:nrow(x)) {
                if (i < j) {
                    tmp <- matrix(0, nrow = nrow(x), ncol = 1)
                    tmp[i, ] <- -1
                    tmp[j, ] <- 1
                    mat <- c(mat, list(tmp))
                    if (isTRUE(flag)) {
                        lab_col <- c(lab_col, sprintf("Row %s - Row %s", j, i))
                    } else {
                        lab_col <- c(lab_col, sprintf("%s - %s", lab_row[j], lab_row[i]))
                    }
                }
            }
        }
        lincom <- do.call("cbind", mat)
        colnames(lincom) <- lab_col
    }

    if (isTRUE(hypothesis == "pairwise")) {
        lab_row <- get_hypothesis_row_labels(x, by = by)
        lab_col <- NULL
        flag <- length(lab_row) == 0 || anyDuplicated(lab_row) > 0
        mat <- list()
        for (i in seq_len(nrow(x))) {
            for (j in 2:nrow(x)) {
                if (i < j) {
                    tmp <- matrix(0, nrow = nrow(x), ncol = 1)
                    tmp[j, ] <- -1
                    tmp[i, ] <- 1
                    mat <- c(mat, list(tmp))
                    if (isTRUE(flag)) {
                        lab_col <- c(lab_col, sprintf("Row %s - Row %s", i, j))
                    } else {
                        lab_col <- c(lab_col, sprintf("%s - %s", lab_row[i], lab_row[j]))
                    }
                }
            }
        }
        lincom <- do.call("cbind", mat)
        colnames(lincom) <- lab_col
    }

    # we assume this is a string formula
    if (is.character(hypothesis) && is.null(lincom)) {

        # row indices: `hypotheses` includes them, but `term` does not
        if (isTRUE(grepl("\\bb\\d+\\b", hypothesis)) && !any(grepl("\\bb\\d+\\b", x[["term"]]))) {
            lab <- hypothesis
            for (i in seq_len(nrow(x))) {
                tmp <- paste0("marginaleffects__", i)
                hypothesis <- gsub(paste0("b", i), tmp, hypothesis)
            }
            rowlabels <- paste0("marginaleffects__", seq_len(nrow(x)))

        # term names
        } else {
            if (!"term" %in% colnames(x) || anyDuplicated(x$term) > 0) {
                msg <- c(
                'To use term names in a `hypothesis` string, the same function call without `hypothesis` argument must produce a `term` column with unique row identifiers. You can use `b1`, `b2`, etc. indices instead of term names in the `hypotheses` string Ex: "b1 + b2 = 0" Alternatively, you can use the `newdata`, `variables`, or `by` arguments:',
                "",
                'mod <- lm(mpg ~ am * vs + cyl, data = mtcars)',
                'comparisons(mod, newdata = "mean", hypothesis = "b1 = b2")',
                'comparisons(mod, newdata = "mean", hypothesis = "am = vs")',
                'comparisons(mod, variables = "am", by = "cyl", hypothesis = "pairwise")')
                insight::format_error(msg)
            }
            rowlabels <- x$term
        }

        eval_string_function <- function(vec, hypothesis, rowlabels) {
            envir <- parent.frame()
            void <- sapply(
                seq_along(vec), function(i)
                assign(rowlabels[i], vec[i], envir = envir))
            out <- eval(parse(text = hypothesis), envir = envir)
            return(out)
        }

        draws <- attr(x, "posterior_draws")
        if (!is.null(draws)) {
            insight::check_if_installed("collapse", minimum_version = "1.9.0")
            tmp <- apply(
                draws,
                MARGIN = 2,
                FUN = eval_string_function,
                hypothesis = hypothesis,
                rowlabels = rowlabels)
            draws <- matrix(tmp, ncol = ncol(draws))
            out <- data.table(
                term = gsub("\\s+", "", attr(hypothesis, "label")),
                tmp = collapse::dapply(draws, MARGIN = 1, FUN = collapse::fmedian))


        } else {
            out <- eval_string_function(
                x[["estimate"]],
                hypothesis = hypothesis,
                rowlabels = rowlabels)
            out <- data.table(
                term = gsub("\\s+", "", attr(hypothesis, "label")),
                tmp = out)
        }

        setnames(out, old = "tmp", new = "estimate")
        attr(out, "posterior_draws") <- draws
        return(out)

    } else if (isTRUE(checkmate::check_numeric(lincom))) {

        # bayesian
        draws <- attr(x, "posterior_draws")
        if (!is.null(draws)) {
            draws <- t(as.matrix(lincom)) %*% draws
            out <- data.table(
                term = colnames(lincom),
                tmp = apply(draws, 1, stats::median))
            setnames(out, old = "tmp", new = "estimate")
            attr(out, "posterior_draws") <- draws

        # frequentist
        } else {
            out <- data.table(
                term = colnames(lincom),
                tmp = as.vector(x[["estimate"]] %*% lincom))
            setnames(out, old = "tmp", new = "estimate")
        }

        out <- out[out$term != "1 - 1", , drop = FALSE]
        return(out)
    }

    msg <- 
    "`hypotheses` is broken. Please report this bug:
    https://github.com/vincentarelbundock/marginaleffects/issues."
    stop(msg, call. = FALSE)
}



get_hypothesis_row_labels <- function(x, by = NULL) {
    lab <- grep("^term$|^by$|^group$|^value$|^contrast$|^contrast_", colnames(x), value = TRUE)
    lab <- Filter(function(z) length(unique(x[[z]])) > 1, lab)
    if (isTRUE(checkmate::check_character(by))) {
        lab <- unique(c(lab, by))
    }
    if (length(lab) == 0) {
        lab <- NULL
    } else {
        lab <- apply(data.frame(x)[, lab, drop = FALSE], 1, paste, collapse = ",")
    }

    # wrap in parentheses to avoid a-b-c-d != (a-b)-(c-d)
    if (any(grepl("-", lab))) {
        lab <- sprintf("(%s)", lab)
    }

    return(lab)
}
