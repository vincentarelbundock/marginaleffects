get_hypothesis <- function(
    x,
    hypothesis,
    by = NULL,
    newdata = NULL,
    draws = NULL
) {
    deprecated <- c(
        "pairwise",
        "revpairwise",
        "sequential",
        "revsequential",
        "reference",
        "meandev",
        "meanotherdev",
        "revreference"
    )
    if (isTRUE(checkmate::check_choice(hypothesis, deprecated))) {
        msg <- "This string is deprecated for use in the `hypothesis` argument. Use the formula interface instead. Ex: `hypothesis=~reference`"
        stop(msg, call. = FALSE)
    }

    # otherwise setDT strips out the marginaleffects-specific classes
    x <- data.table::copy(x)

    vec <- isTRUE(checkmate::check_atomic_vector(hypothesis)) &&
        isTRUE(checkmate::check_numeric(hypothesis))
    mat <- isTRUE(checkmate::check_matrix(hypothesis))

    if (is.null(hypothesis)) {
        data.table::setDT(x)
        return(x)
    } else if (isTRUE(checkmate::check_formula(hypothesis))) {
        out <- hypothesis_formula(
            x,
            newdata = newdata,
            hypothesis = hypothesis,
            by = by
        )
        return(out)
    } else if (isTRUE(checkmate::check_function(hypothesis))) {
        out <- hypothesis_function(
            x,
            newdata = newdata,
            hypothesis = hypothesis,
            by = by
        )
        return(out)
    } else if (vec || mat) {
        out <- hypothesis_matrix(x, hypothesis = hypothesis)
        return(out)
    } else if (is.character(hypothesis)) {
        out <- hypothesis_string(x, hypothesis = hypothesis)
        return(out)
    }

    stop(
        "`hypothesis` is broken. Please report this bug with a reproducible example: https://github.com/vincentarelbundock/marginaleffects/issues.",
        call. = FALSE
    )
}
