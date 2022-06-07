get_lincom <- function(x, lincom, column) {

    if (is.null(lincom)) {
        return(x)
    }

    # must be checked here when we know how many rows the output has
    if (isTRUE(lincom %in% c("pairwise", "reference"))) {
        if (nrow(x) > 25) {
            msg <- format_msg(
            'The "pairwise" option of the `lincom` argument is not supported for
            `marginaleffects` commands which generate more than 25 rows of results. Use the
            `newdata` and/or the `variables` arguments to compute a smaller set of
            results.')
            stop(msg, call. = FALSE)
        }
    }

    if (isTRUE(checkmate::check_numeric(lincom)) && isTRUE(checkmate::check_atomic_vector(lincom))) {
        if (length(lincom) != nrow(x)) {
            msg <- sprintf(
            "The `lincom` vector must be of length %s.", nrow(x))
            stop(msg, call. = FALSE)
        }
        out <- data.table(
            term = "lincom",
            lincom = "custom",
            tmp = as.vector(x[[column]] %*% lincom))
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    if (isTRUE(checkmate::check_matrix(lincom))) {
        if (nrow(lincom) != nrow(x)) {
            msg <- sprintf(
            "The `lincom` matrix must be have %s rows.", nrow(x))
            stop(msg, call. = FALSE)
        }
        out <- data.table(
            term = "lincom",
            lincom = "custom",
            tmp = as.vector(x[[column]] %*% lincom))
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    if (isTRUE(lincom == "reference")) {
        lab <- NULL
        mat <- list()
        for (j in 2:nrow(x)) {
            tmp <- matrix(0, nrow = nrow(x), ncol = 1)
            tmp[1, ] <- -1
            tmp[j, ] <- 1
            mat <- c(mat, list(tmp))
            lab <- c(lab, paste0(j, " - ", 1))
        }
        lc <- do.call("cbind", mat)
        out <- data.table(
            term = "lincom",
            lincom = lab,
            tmp = as.vector(x[[column]] %*% lc))
        out <- out[out$term != "1 - 1", , drop = FALSE]
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    if (isTRUE(lincom == "pairwise")) {
        lab <- NULL
        mat <- list()
        for (i in 1:nrow(x)) {
            for (j in 2:nrow(x)) {
                if (i < j) {
                    tmp <- matrix(0, nrow = nrow(x), ncol = 1)
                    tmp[i, ] <- -1
                    tmp[j, ] <- 1
                    mat <- c(mat, list(tmp))
                    lab <- c(lab, paste0(j, " - ", i))
                }
            }
        }
        lc <- do.call("cbind", mat)
        out <- data.table(
            term = "lincom",
            lincom = lab,
            tmp = as.vector(x[[column]] %*% lc))
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    # we assume this is a string formula
    if (is.character(lincom)) {
        envir <- parent.frame()

        # row indices: `lincom` includes them, but `term` does not
        if (isTRUE(grepl("\\br\\d+\\b", lincom)) && !any(grepl("\\br\\d+\\b", x[["term"]]))) {
            lab <- lincom
            for (i in seq_len(nrow(x))) {
                tmp <- paste0("marginaleffects__", i)
                lincom <- gsub(paste0("r", i), tmp, lincom)
                assign(tmp, x[[column]][i], envir = envir)
            }

        # term names
        } else {
            if (anyDuplicated(x$term) > 0) {
                msg <- format_msg(
                "There are duplicate term names. Please use row indices
                instead of term names in the the `lincom` formula. Ex:
                `r1 + r2 = 0`")
                stop(msg, call. = FALSE)
            }

            for (i in seq_len(nrow(x))) {
                tmp <- x$term[i]
                assign(tmp, x[[column]][i], envir = envir)
            }
        }

        out <- eval(parse(text = lincom), envir = envir)
        out <- data.table(
            term = "lincom",
            lincom = gsub("\\s+", "", attr(lincom, "label")),
            tmp = out)
        setnames(out, old = "tmp", new = column)
        return(out)
    }

    msg <- 
    "`lincom` is broken. Please report this bug:
    https://github.com/vincentarelbundock/marginaleffects/issues."
    stop(msg, call. = FALSE)

}

