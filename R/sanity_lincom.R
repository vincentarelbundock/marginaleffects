sanity_lincom <- function(lincom) {
    checkmate::assert(
        checkmate::check_null(lincom),
        checkmate::check_numeric(lincom),
        checkmate::check_matrix(lincom),
        checkmate::check_choice(lincom, choices = c("reference", "pairwise")))
}
