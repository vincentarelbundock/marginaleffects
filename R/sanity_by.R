sanity_by <- function(mfx, by) {
    checkmate::assert(
        checkmate::check_flag(by),
        checkmate::check_data_frame(by, min.cols = 1, min.rows = 1),
        checkmate::check_character(by, min.len = 1),
        checkmate::check_null(by)
    )

    known <- c(
        "by",
        "group",
        "term",
        "rowid",
        "rowidcf",
        "contrast",
        colnames(mfx@newdata)
    )

    if (isTRUE(by == "group") && "group" %in% colnames(mfx@newdata)) {
        msg <- 'The "group" variable name is forbidden to avoid conflicts with the column names of the outputs produced by the `marginaleffects` package. Please rename your variable of change the value of the `by` argument.'
        stop_sprintf(msg)
    }

    if (isTRUE(checkmate::check_flag(by))) {
        flag <- FALSE
    } else if (inherits(by, "data.frame")) {
        flag <- !all(colnames(by) %in% known) || !"by" %in% colnames(by)
    } else {
        flag <- !all(by %in% known | grepl("^contrast_", by))
    }

    if (flag) {
        bycols <- toString(setdiff(
            colnames(mfx@newdata),
            c("rowid", "rowidcf", "term", "group")
        ))
        msg <- c(
            "The `by` argument must be either:",
            "",
            sprintf(
                "1. Character vector in which each element is part of: %s",
                bycols
            ),
            "",
            sprintf(
                "2. A data frame with a `by` column of labels, and in which all other columns are elements of: %s",
                bycols
            ),
            "",
            "It can sometimes be useful to supply a data frame explicitly to the `newdata` argument in order to be able to group by different columns."
        )
        stop(insight::format_message(msg), call. = FALSE)
    }
}

add_by <- function(mfx, by) {
    sanity_by(mfx, by)

    if (isTRUE(checkmate::check_data_frame(by))) {
        mfx@variable_names_by <- colnames(by)
    } else if (isTRUE(checkmate::check_character(by))) {
        mfx@variable_names_by <- by
    }

    return(mfx)
}
