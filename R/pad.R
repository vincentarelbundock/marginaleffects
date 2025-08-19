#' Pad newdata with factor levels and merge with original data
#'
#' `model.matrix` breaks when `newdata` includes a factor
#' variable, but not all levels are present in the data. This is bad for us
#' because we often want to get predictions with one (or few) rows, where some
#' factor levels are inevitably missing.
#' @param model Model object to check for mlogit class
#' @param newdata Data frame to pad
#' @keywords internal
pad <- function(model, newdata) {
    checkmate::assert_data_frame(newdata)

    # pad factors: `model.matrix` breaks when factor levels are missing
    # support `newdata` and assume no padding the `idx` column is necessary for
    # `get_predict` but it breaks binding, so we can't remove it in
    # sanity_newdata and we can't rbind it with padding
    # pad factors: `model.matrix` breaks when factor levels are missing
    if (inherits(model, "mlogit")) {
        return(newdata)
    }

    # store variables with missing factors
    vault <- list()
    for (v in colnames(newdata)) {
        if (is.factor(newdata[[v]])) {
            if (!all(levels(newdata[[v]]) %in% newdata[[v]])) {
                vault[[v]] <- factor(levels(newdata[[v]]), levels = levels(newdata[[v]]))
            }
        }
    }

    # create padding
    if (length(vault) > 0) {
        # HACK: Some models use a character variable with many levels (e.g.,
        # mixed-effects groups). This creates a massive padding dataset, and making
        # predictions can become very expensive.
        if (isTRUE(sum(lengths(vault)) > 100)) {
            return(newdata)
        }

        padding <- utils::head(newdata, 1)
        data.table::setDT(padding)
        for (v in names(vault)) {
            padding[[v]] <- NULL
        }
        gr <- do.call(data.table::CJ, vault)
        padding <- cjdt(list(padding, gr))
        to_keep <- colnames(newdata)
        padding[, ..to_keep]
        setcolorder(padding, to_keep)
        data.table::setDF(padding)

        padding$rowid <- -1 * padding$rowid

        # merge padding with original newdata
        return(rbindlist(list(padding, newdata)))
    } else {
        return(newdata)
    }
}

# unpad
unpad <- function(out, draws) {
    if (!"rowid" %in% colnames(out)) {
        return(out)
    }
    idx <- out$rowid > 0
    out <- out[idx, , drop = FALSE]
    if (!is.null(draws)) {
        draws <- draws[idx, , drop = FALSE]
    }
    return(list(out = out, draws = draws))
}
