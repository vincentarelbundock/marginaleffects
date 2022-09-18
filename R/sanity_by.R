sanity_by <- function(by, newdata) {
    checkmate::assert(
        checkmate::check_data_frame(by, min.cols = 1, min.rows = 1),
        checkmate::check_character(by, min.len = 1),
        checkmate::check_null(by))

    known <- c("by", "group", "term", "rowid", "rowidcf", colnames(newdata))

    if (inherits(by, "data.frame")) {
        flag <- !all(colnames(by) %in% known) || !"by" %in% colnames(by)
    } else {
        flag <- !all(by %in% known)
    }

    if (flag) {
        bycols <- paste(setdiff(colnames(newdata), c("rowid", "rowidcf", "term")), collapse = ", ")
        msg <- c(
            "The `by` argument must be either:", "",
            sprintf("1. Character vector in which each element is part of: %s", bycols),
            "",
            sprintf("2. A data frame with a `by` column of labels, and in which all other columns are elements of: %s", bycols),
            "",
            "It can sometimes be useful to supply a data frame explicitly to the `newdata` argument in order to be able to group by different columns."
        )
        stop(insight::format_message(msg), call. = FALSE)
    }
}