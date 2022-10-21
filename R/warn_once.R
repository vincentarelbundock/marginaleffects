warn_once <- function(msg, id) {
    msg <- c(msg, "", "This warning appears once per session.")
    if (isTRUE(getOption(id, default = TRUE))) {
        insight::format_warning(msg, call. = FALSE)
        opts <- list(FALSE)
        names(opts) <- id
        options(opts)
    }
}
