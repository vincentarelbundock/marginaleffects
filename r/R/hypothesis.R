get_hypothesis <- function(
    x,
    hypothesis,
    by = NULL,
    newdata = NULL,
    draws = NULL,
    mfx = NULL) {
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
    rowplan_assert_draws(x, attr(x, "posterior_draws"), "hypothesis input")

    vec <- isTRUE(checkmate::check_atomic_vector(hypothesis)) &&
        isTRUE(checkmate::check_numeric(hypothesis))
    mat <- isTRUE(checkmate::check_matrix(hypothesis))

    out <- if (is.null(hypothesis)) {
        x
    } else if (isTRUE(checkmate::check_formula(hypothesis))) {
        hypothesis_formula(
            x,
            newdata = newdata,
            hypothesis = hypothesis,
            by = by,
            mfx = mfx
        )
    } else if (isTRUE(checkmate::check_function(hypothesis))) {
        hypothesis_function(
            x,
            newdata = newdata,
            hypothesis = hypothesis,
            by = by
        )
    } else if (vec || mat) {
        hypothesis_matrix(x, hypothesis = hypothesis)
    } else if (is.character(hypothesis)) {
        hypothesis_string(x, hypothesis = hypothesis)
    } else {
        stop(
            "`hypothesis` is broken. Please report this bug with a reproducible example: https://github.com/vincentarelbundock/marginaleffects/issues.",
            call. = FALSE
        )
    }

    rowplan_assert_draws(out, attr(out, "posterior_draws"), "hypothesis")

    return(out)
}
