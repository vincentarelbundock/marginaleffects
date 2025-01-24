get_term_labels <- function(x, idx = NULL) {
  if (is.data.frame(x)) {
    if ("term" %in% names(x) && anyDuplicated(x$term) == 0L) {
      return(unique(x$term))
    } else if (any(grepl("^contrast|^group$", names(x)))) {
      tmp <- grep("^term$|^contrast|^group$", names(x), value = TRUE)

      by <- attr(x, "by")
      if (isTRUE(checkmate::check_character(by))) {
        tmp <- unique(c(tmp, by))
      }

      tmp <- intersect(tmp, names(x))

      out <- data.table(x)[, ..tmp, drop = FALSE]
      out <- lapply(out, function(x) if (length(unique(x)) == 1) NULL else x)
      if (length(out) == 0) {
        out <- paste0("b", seq_len(nrow(x)))
      } else {
        out <- do.call(paste, c(out, sep = "_"))
        if (anyDuplicated(out) > 0) {
          out <- paste0("b", seq_len(nrow(x)))
        }
      }
      out <- trimws(out)
    } else if (inherits(x, "predictions")) {
      by <- attr(x, "by")
      if (isTRUE(checkmate::check_character(by)) && all(by %in% names(x))) {
        out <- apply(x[, by, drop = FALSE], 1, paste, collapse = "_")
        if (anyDuplicated(out) > 0) {
          out <- paste0("b", seq_len(nrow(x)))
        }
      } else {
        out <- paste0("b", seq_len(nrow(x)))
      }
    } else {
      out <- paste0("b", seq_len(nrow(x)))
    }
  } else if (is.vector(x)) {
    if (!is.null(names(x))) {
      out <- names(x)
    } else {
      out <- paste0("b", seq_along(x))
    }
  } else {
    return(NULL)
  }
  if (!is.null(idx)) out <- out[idx]
  return(out)
}
