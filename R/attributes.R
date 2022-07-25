get_attributes <- function(x, exclude = NULL, include = NULL, include_regex = NULL) {
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

set_attributes <- function(x, attr_cache, prefix = "") {
    for (n in names(attr_cache)) {
        attr(x, paste0(prefix, n)) <- attr_cache[[n]]
    }
    return(x)
}
