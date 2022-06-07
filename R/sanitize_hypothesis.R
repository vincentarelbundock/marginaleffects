sanitize_hypothesis <- function(hypothesis, ...) {

    deprecation_arg(hypothesis, "hypothesis", "lincom", ...) 

    checkmate::assert(
        checkmate::check_string(hypothesis, pattern = "="),
        checkmate::check_choice(hypothesis, choices = c("reference", "pairwise")),
        checkmate::check_numeric(hypothesis),
        checkmate::check_matrix(hypothesis),
        checkmate::check_null(hypothesis))

    if (isTRUE(checkmate::check_string(hypothesis, pattern = "="))) {
        out <- paste(gsub("=", "-(", hypothesis), ")")
        attr(out, "label") <- hypothesis
        return(out)
    } else {
        return(hypothesis)
    }

}
