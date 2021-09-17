#' Create a data.frame with all factor levels
#'
#' `model.matrix` and `get_predicted` break when `newdata` includes a factor
#' variable, but not all levels are present in the data. This is bad for us
#' because we often want to get predictions with one (or few) rows, where some
#' factor levels are inevitably missing.
#' @keywords internal
pad_factors <- function(x) {
    checkmate::assert_data_frame(x)
    # store variables with missing factors
    vault <- list()
    for (v in colnames(x)) {
        if (is.factor(x[[v]])) {
            if (!all(levels(x[[v]]) %in% x[[v]])) {
                vault[[v]] <- factor(levels(x[[v]]), levels = levels(x[[v]]))
            }
        }
    }
    # create padding
    if (length(vault) > 0) {
        padding <- head(x, 1)
        for (v in names(vault)) {
            padding[[v]] <- NULL
        }
        padding <- merge(padding, expand.grid(vault), all = TRUE)
        padding <- padding[, colnames(x)]
    } else {
        padding <- data.frame()
    }
    return(padding)
}
