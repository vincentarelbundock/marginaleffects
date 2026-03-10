add_hypothesis <- function(mfx, hypothesis) {
    checkmate::assert(
        checkmate::check_character(hypothesis, pattern = "=|<=|>="),
        checkmate::check_numeric(hypothesis),
        checkmate::check_formula(hypothesis),
        checkmate::check_matrix(hypothesis),
        checkmate::check_function(hypothesis),
        checkmate::check_null(hypothesis)
    )

    hypothesis_direction <- "="
    hnull <- 0

    # Issue #1453: ratio ~ should use hypothesis=1 as null
    if (isTRUE(checkmate::check_formula(hypothesis))) {
        if (isTRUE(identical("ratio", as.character(hypothesis)[2]))) {
            hnull <- 1
        }
    }

    if (isTRUE(checkmate::check_character(hypothesis))) {
        # Extract direction for each hypothesis string
        hypothesis_direction <- rep("=", length(hypothesis))
        hypothesis_direction[grepl("<=", hypothesis)] <- "<="
        hypothesis_direction[grepl(">=", hypothesis)] <- ">="

        # Collapse to single value if all are the same
        if (length(unique(hypothesis_direction)) == 1) {
            hypothesis_direction <- hypothesis_direction[1]
        }

        if (isTRUE(grepl("^=|^<=|^>=", hypothesis))) {
            hypothesis <- sub("^=|^<=|^>=", "", hypothesis)
            hypothesis <- trimws(hypothesis)
            hypothesis <- as.numeric(hypothesis)
        }
    }

    if (isTRUE(checkmate::check_character(hypothesis, pattern = "=|<=|>="))) {
        out <- paste(gsub("=|<=|>=", "-(", hypothesis), ")")
        attr(out, "label") <- hypothesis
        hypothesis <- out
    } else if (isTRUE(checkmate::check_matrix(hypothesis))) {
        attr(hypothesis, "label") <- colnames(hypothesis)
    } else if (isTRUE(checkmate::check_numeric(hypothesis, len = 1))) {
        hnull <- hypothesis
        hypothesis <- NULL
    }

    mfx@hypothesis <- hypothesis
    mfx@hypothesis_null <- hnull
    mfx@hypothesis_direction <- hypothesis_direction

    return(mfx)
}
