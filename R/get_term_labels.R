get_term_labels <- function(x, idx = NULL) {
  if (is.vector(x)) {
    if (!is.null(names(x))) {
      out <- names(x)
    } else {
      out <- paste0("b", seq_along(x))
    }
  
  } else if (is.data.frame(x)) {
    if ("term" %in% names(x) && length(unique(x$term)) == nrow(x)) {
      return(unique(x$term))
    } else if (any(grepl("^contrast", names(x)))) {
      tmp <- grep("^term$|^contrast", names(x))
      out <- x[, tmp, drop = FALSE]
      if (length(unique(out[["term"]])) == 1) {
        out[["term"]] <- NULL
      }
      out <- apply(out, 1, paste, collapse = " ")
    } else {
      out <- paste0("b", seq_len(nrow(x)))
    }
  } else {
    return(NULL)
  }
  if (!is.null(idx)) out <- out[idx]
  return(out)
}
