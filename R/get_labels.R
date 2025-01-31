get_labels <- function(x, idx = NULL, by = NULL, wrap_parens = FALSE, hypothesis_by = NULL) {
    if (!is.data.frame(x) && !is.vector(x)) {
        return(NULL)
    }

    if (is.data.frame(x)) {
        x <- data.table::data.table(x)

        # Identify relevant columns
        lab_cols <- grep("^term$|^group$|^contrast$|^contrast_|^value$|^by$", names(x), value = TRUE)
        if (isTRUE(checkmate::check_character(by))) {
            lab_cols <- unique(c(lab_cols, by))
        }

        lab_cols <- setdiff(lab_cols, hypothesis_by)

        # Filter out columns with more than one unique value, within `hypothesis_by`
        lab_cols <- Filter(function(col) length(unique(x[[col]])) > 1, lab_cols)

        if (length(lab_cols) == 0) {
            # Default labels if no meaningful columns found
            labels <- paste0("b", seq_len(nrow(x)))
        } else {
            # Create labels by pasting unique combinations of selected columns
            lab_df <- x[, ..lab_cols]
            labels <- apply(lab_df, 1, paste, collapse = " ")

            # duplicated labels (within groups) revert to b1, b2, ...
            uniq <- TRUE
            if (isTRUE(checkmate::check_character(hypothesis_by, min.len = 1))) {
                uniq <- x[, ..hypothesis_by]
                uniq[, marginaleffects_unique_labels := labels]
                uniq <- uniq[,
                    .(marginaleffects_uniq_labels = anyDuplicated(marginaleffects_unique_labels) > 0),
                    by = hypothesis_by]
                uniq <- !any(uniq$V1)
            } else if (anyDuplicated(labels) > 0) {
                uniq <- FALSE
            }

            if (!isTRUE(uniq)) {
                labels <- paste0("b", seq_len(nrow(x)))
            }
        }
    } else if (is.vector(x)) {
        # For vectors
        if (!is.null(names(x))) {
            labels <- names(x)
        } else {
            labels <- paste0("b", seq_along(x))
        }
    } else {
        return(NULL)
    }

    # Wrap labels in parentheses if requested
    if (wrap_parens && any(grepl("-", labels))) {
        labels <- sprintf("(%s)", labels)
    }

    # Subset labels if idx is provided
    if (!is.null(idx)) {
        labels <- labels[idx]
    }

    return(labels)
}
