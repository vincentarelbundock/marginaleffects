get_lincom <- function(x, lincom, column) {

    if (is.null(lincom)) {
        return(x)
    }

    if (isTRUE(checkmate::check_numeric(lincom)) || isTRUE(checkmate::check_matrix(lincom))) {
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

    if (is.character(lincom)) {
        envir <- parent.frame()

        if (isTRUE(grepl("\\bx\\d", lincom))) {
            lab <- lincom
            for (i in seq_len(nrow(x))) {
                tmp <- paste0("marginaleffects__", i)
                lincom <- gsub(paste0("x", i), tmp, lincom)
                assign(tmp, x[[column]][i], envir = envir)
            }
        } else {
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

    stop("`lincom` is broken. Please report this bug: https://github.com/vincentarelbundock/marginaleffects/issues.", call. = FALSE)

}
