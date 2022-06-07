sanitize_lincom <- function(lincom) {
    checkmate::assert(
        checkmate::check_null(lincom),
        checkmate::check_numeric(lincom),
        checkmate::check_matrix(lincom),
        checkmate::check_choice(lincom, choices = c("reference", "pairwise")),
        checkmate::check_string(lincom, pattern = "="))

    if (isTRUE(checkmate::check_string(lincom, pattern = "="))) {
        out <- paste(gsub("=", "-(", lincom), ")")
        attr(out, "label") <- lincom
        return(out)
    } else {
        return(lincom)
    }
}
