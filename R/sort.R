sort_columns <- function(x, newdata = data.frame(), by = NULL) {
    if (!inherits(x, "data.table")) {
        data.table::setDT(x)
    }
    if (isTRUE(checkmate::check_character(by))) {
        bycols <- by
    } else if (isTRUE(checkmate::check_data_frame(by))) {
        bycols <- colnames(by)
    } else {
        bycols <- NULL
    }
    stubcols <- c(
        "rowid",
        "rowidcf",
        "term",
        "group",
        "hypothesis",
        "by",
        grep("^contrast", colnames(x), value = TRUE),
        bycols,
        "estimate",
        "std.error",
        "statistic",
        "p.value",
        "s.value",
        "conf.low",
        "conf.high",
        attr(newdata, "newdata_variables_datagrid"),
        "marginaleffects_wts",
        sort(grep("^predicted", colnames(newdata), value = TRUE))
    )
    cols <- intersect(stubcols, colnames(x))
    cols <- unique(c(cols, colnames(x)))
    x <- x[, ..cols]
    if ("group" %in% names(x) && all(x$group == "main_marginaleffect")) {
        x$group <- NULL
    }
    return(x)
}
