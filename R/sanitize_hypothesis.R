sanitize_hypothesis <- function(hypothesis, ...) {
    checkmate::assert(
        checkmate::check_character(hypothesis, pattern = "="),
        checkmate::check_character(hypothesis, pattern = "^[><][0-9. -]+$"),
        checkmate::check_numeric(hypothesis),
        checkmate::check_formula(hypothesis),
        checkmate::check_matrix(hypothesis),
        checkmate::check_function(hypothesis),
        checkmate::check_null(hypothesis)
    )

    if (
        isTRUE(checkmate::check_character(
            hypothesis,
            pattern = "^[><][0-9. -]+$"
        ))
    ) {
        out = list(
            hypothesis = NULL,
            hypothesis_null = as.numeric(sub("^[><]", "", trimws(hypothesis))),
            hypothesis_direction = substr(hypothesis, 1, 1)
        )
        return(out)
    }

    hnull <- 0

    if (isTRUE(checkmate::check_character(hypothesis, pattern = "="))) {
        out <- paste(gsub("=", "-(", hypothesis), ")")
        attr(out, "label") <- hypothesis
        hypothesis <- out
    } else if (isTRUE(checkmate::check_matrix(hypothesis))) {
        attr(hypothesis, "label") <- colnames(hypothesis)
    } else if (isTRUE(checkmate::check_numeric(hypothesis, len = 1))) {
        hnull <- hypothesis
        hypothesis <- NULL
    }

    out <- list(
        "hypothesis" = hypothesis,
        "hypothesis_null" = hnull,
        "hypothesis_direction" = "="
    )

    return(out)
}
