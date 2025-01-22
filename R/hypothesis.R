get_hypothesis <- function(
    x,
    hypothesis,
    by = NULL,
    newdata = NULL,
    draws = NULL) {
    deprecated <- c("pairwise", "revpairwise", "sequential", "revsequential", "reference", "meandev", "meanotherdev", "revreference")
    if (isTRUE(checkmate::check_choice(hypothesis, deprecated))) {
        msg <- "This string is deprecated for use in the `hypothesis` argument. Use the formula interface instead. Ex: `hypothesis=~reference`"
        stop(msg, call. = FALSE)
    }

    vec <- isTRUE(checkmate::check_atomic_vector(hypothesis)) &&
        isTRUE(checkmate::check_numeric(hypothesis))
    mat <- isTRUE(checkmate::check_matrix(hypothesis))

    if (is.null(hypothesis)) {
        data.table::setDT(x)
        return(x)
    } else if (isTRUE(checkmate::check_formula(hypothesis))) {
        out <- hypothesis_formula(x, newdata = newdata, hypothesis = hypothesis, by = by)
        return(out)
    } else if (isTRUE(checkmate::check_function(hypothesis))) {
        out <- hypothesis_function(x, newdata = newdata, hypothesis = hypothesis, by = by)
        return(out)
    } else if (vec || mat) {
        out <- hypothesis_matrix(x, hypothesis = hypothesis)
        return(out)
    } else if (is.character(hypothesis)) {
        out <- hypothesis_string(x, hypothesis = hypothesis)
        return(out)
    }

    stop("`hypothesis` is broken. Please report this bug with a reproducible example: https://github.com/vincentarelbundock/marginaleffects/issues.", call. = FALSE)
}


get_hypothesis_row_labels <- function(x, by = NULL) {
    lab <- grep("^term$|^by$|^group$|^value$|^contrast$|^contrast_", colnames(x), value = TRUE)
    lab <- Filter(function(z) length(unique(x[[z]])) > 1, lab)
    if (isTRUE(checkmate::check_character(by))) {
        lab <- unique(c(lab, by))
    }
    if (length(lab) == 0) {
        lab <- NULL
    } else {
        lab_df <- data.frame(x)[, lab, drop = FALSE]
        idx <- vapply(lab_df, FUN = function(x) length(unique(x)) > 1, FUN.VALUE = logical(1))
        if (sum(idx) > 0) {
            lab <- apply(lab_df[, idx, drop = FALSE], 1, paste, collapse = ", ")
        } else {
            lab <- apply(lab_df, 1, paste, collapse = ", ")
        }
    }

    # wrap in parentheses to avoid a-b-c-d != (a-b)-(c-d)
    if (any(grepl("-", lab))) {
        lab <- sprintf("(%s)", lab)
    }

    return(lab)
}
