#' Create a data.frame with all factor or character levels
#'
#' `model.matrix` breaks when `newdata` includes a factor
#' variable, but not all levels are present in the data. This is bad for us
#' because we often want to get predictions with one (or few) rows, where some
#' factor levels are inevitably missing.
#' @keywords internal
complete_levels <- function(x, character_levels = NULL) {

    checkmate::assert_data_frame(x)

    # fixest returned an empty list()
    if (is.null(character_levels) || length(character_levels) == 0) {
        return(data.frame())
    }

    # store variables with missing factors or characters
    vault <- list()
    for (v in colnames(x)) {
        if (is.factor(x[[v]])) {
            if (!all(levels(x[[v]]) %in% x[[v]])) {
                vault[[v]] <- factor(levels(x[[v]]), levels = levels(x[[v]]))
            }
        } else if (is.character(x[[v]])) {
            if (v %in% names(character_levels)) {
                vault[[v]] <- character_levels[[v]]
            }
        }
    }

    # create padding
    if (length(vault) > 0) {
        # HACK: Some models use a character variable with many levels (e.g.,
        # mixed-effects groups). This creates a massive padding dataset, and making
        # predictions can become very expensive.
        if (isTRUE(sum(lengths(vault)) > 100)) {
            return(data.frame())
        }

        padding <- utils::head(x, 1)
        data.table::setDT(padding)
        for (v in names(vault)) {
            padding[[v]] <- NULL
        }
        fun <- data.table::CJ
        gr <- do.call("fun", vault)
        padding <- cjdt(list(padding, gr))
        to_keep <- colnames(x)
        padding[, ..to_keep]
        setcolorder(padding, to_keep)
        data.table::setDF(padding)
    } else {
        padding <- data.frame()
    }

    padding$rowid <- -1 * padding$rowid

    return(padding)
}
