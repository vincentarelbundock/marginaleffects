#' Create a data.frame with all factor or character levels
#'
#' `model.matrix` and `get_predicted` break when `newdata` includes a factor
#' variable, but not all levels are present in the data. This is bad for us
#' because we often want to get predictions with one (or few) rows, where some
#' factor levels are inevitably missing.
#' @keywords internal
complete_levels <- function(x, levels_character = NULL) {
    checkmate::assert_data_frame(x)
    # store variables with missing factors or characters
    vault <- list()
    for (v in colnames(x)) {
        if (is.factor(x[[v]])) {
            if (!all(levels(x[[v]]) %in% x[[v]])) {
                vault[[v]] <- factor(levels(x[[v]]), levels = levels(x[[v]]))
            }
        } else if (is.character(x[[v]])) {
            if (v %in% names(levels_character)) {
                vault[[v]] <- levels_character[[v]]
            }
        }
    }
    # create padding
    if (length(vault) > 0) {
        padding <- utils::head(x, 1)
        for (v in names(vault)) {
            padding[[v]] <- NULL
        }
        padding <- merge(padding, expand.grid(vault), all = TRUE)
        padding <- padding[, colnames(x)]
    } else {
        padding <- data.frame()
    }

    # negative rowid for easy drop
    if (nrow(padding) > 0) {
        padding$rowid <- -1 * 1:nrow(padding)
    }
    
    return(padding)
}
