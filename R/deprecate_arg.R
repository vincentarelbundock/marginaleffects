deprecation_arg <- function(x, newname, oldname, ...) {
    dots <- list(...)
    if (oldname %in% names(dots)) {
        msg <- sprintf(format_msg(
        "The `%s` argument was replaced by the `%s` argument."),
        oldname, newname)
        warning(msg, call. = FALSE)
    }
    if (is.null(x)) {
        out <- dots[[oldname]]
    } else {
        out <- x
    }
    return(out)
}
