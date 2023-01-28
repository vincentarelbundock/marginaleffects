sort_columns <- function(x, first = NULL, alpha = FALSE) {
    cols <- colnames(x)

    forget <- c("names", "row.names", "class")
    attr_save <- attributes(x)
    attr_save <- attr_save[!names(attr_save) %in% forget]

    if (isTRUE(alpha)) {
        cols <- sort(cols)
    }

    if (!is.null(first)) {
        cols <- unique(c(first, cols))
    }

    cols <- intersect(cols, colnames(x))

    if (inherits(x, "data.table")) {
        out <- x[, ..cols, drop = FALSE]
    } else {
        out <- x[, cols, drop = FALSE]
    }

    for (n in names(attr_save)) {
        attr(out, n) <- attr_save[[n]]
    }

    return(out)
}

get_marginaleffects_attributes <- function(x, exclude = NULL, include = NULL, include_regex = NULL) {
    out <- list()
    attr_names <- names(attributes(x))
    attr_names <- setdiff(attr_names, exclude)
    if (!is.null(include)) attr_names <- intersect(attr_names, include)
    if (!is.null(include_regex)) attr_names <- attr_names[grepl(include_regex, attr_names)]
    for (n in attr_names) {
        out[[n]] <- attr(x, n)
    }
    return(out)
}

set_marginaleffects_attributes <- function(x, attr_cache, prefix = "") {
    for (n in names(attr_cache)) {
        attr(x, paste0(prefix, n)) <- attr_cache[[n]]
    }
    return(x)
}


warn_once <- function(msg, id) {
    msg <- c(msg, "", "This warning appears once per session.")
    if (isTRUE(getOption(id, default = TRUE))) {
        insight::format_warning(msg, call. = FALSE)
        opts <- list(FALSE)
        names(opts) <- id
        options(opts)
    }
}


# Cross join a list of data tables
# Source: https://github.com/Rdatatable/data.table/issues/1717#issuecomment-545758165
cjdt <- function(dtlist) {
    Reduce(function(DT1, DT2) cbind(DT1, DT2[rep(1:.N, each = nrow(DT1))]), dtlist)
}
