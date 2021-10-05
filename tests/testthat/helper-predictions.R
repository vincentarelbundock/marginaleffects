#' internal testing function (no validity check)
#'
#' @export
#' @keywords internal
expect_predictions <- function(object,
                               se = TRUE,
                               n_col = NULL,
                               n_row = NULL) {

  # Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  cl <- class(object)[1]
  ro <- nrow(object)
  co <- ncol(object)
  typecol <- "type" %in% colnames(object)
  predcol <- "predicted" %in% colnames(object)
  secol <- "std.error" %in% colnames(object)
  msg <- sprintf("Class: %s. Rows: %s. Columns: %s. type_col: %s. predicted_col: %s, se_col: %s",
                 cl, ro, co, typecol, predcol, secol)

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

  flag <- cl == "predictions" &&
          isTRUE(n_col) &&
          isTRUE(n_row) &&
          isTRUE(typecol) &&
          isTRUE(predcol) &&
          secol == se

  testthat::expect(isTRUE(flag), msg)

  invisible(act$val)
}
