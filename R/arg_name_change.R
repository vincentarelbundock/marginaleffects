arg_name_change <- function(x, oldname, dots) {
    if (isTRUE(oldname %in% names(dots))) {
        return(dots[[oldname]])
    } else {
        return(x)
    }
}
