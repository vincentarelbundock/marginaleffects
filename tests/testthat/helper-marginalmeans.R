#' internal testing function (no validity check)
#'
#' @export
#' @keywords internal
expect_marginalmeans <- function(object, 
                                 se = TRUE,
                                 n_col = NULL,
                                 n_row = NULL) {

  # Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  cl <- class(object)[1]
  ro <- nrow(object)
  co <- ncol(object)
  predcol <- "marginalmean" %in% colnames(object)
  secol <- "std.error" %in% colnames(object)
  msg <- sprintf("Class: %s. Rows: %s. Columns: %s. predicted_col: %s, se_col: %s",
                 cl, ro, co, predcol, secol)

  if (is.null(n_col)) {
      n_col <- co > 0
  } else {
      n_col <- co == n_col
  }

  if (is.null(n_row)) {
      n_row <- co > 0
  } else {
      n_row <- ro == n_row
  }

  flag <- cl == "marginalmeans" &&
          isTRUE(n_col) &&
          isTRUE(n_row) &&
          isTRUE(predcol) &&
          secol == se

  testthat::expect(isTRUE(flag), msg)

  invisible(act$val)
}
