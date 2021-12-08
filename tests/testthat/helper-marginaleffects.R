#' internal testing function (no validity check)
#'
#' @export
#' @keywords internal
expect_marginaleffects <- function(object,
                                   type = "response",
                                   n_unique = 10,
                                   pct_na = 5,
                                   se = TRUE) {

  # Capture object and label
  act <- testthat::quasi_label(rlang::enquo(object), arg = "object")

  # Compute
  mfx <- marginaleffects(object, type = type)
  tid <- tidy(mfx)

  # Check
  mfx_class <- class(mfx)[1]
  tid_class <- class(tid)[1]
  mfx_nrow <- nrow(mfx)
  tid_nrow <- nrow(tid)
  dydx_unique <- length(unique(round(mfx$dydx, 4))) / length(unique(mfx$term))
  dydx_na <- sum(is.na(mfx$dydx)) / nrow(mfx) * 100
  if (isTRUE(se)) {
      std.error_unique <- length(unique(round(mfx$std.error, 4))) / length(unique(mfx$term))
      std.error_na <- sum(is.na(mfx$std.error_na)) / nrow(mfx) * 100
  } else {
      std.error_unique <- NULL
      std.error_na <- NULL
  }
  msg <- sprintf("Classes: %s, %s. Rows: %s, %s. Unique: %s, %s. NAs: %s, %s.",
                 mfx_class, tid_class,
                 mfx_nrow, tid_nrow,
                 dydx_unique, std.error_unique,
                 sprintf("%.1f%%", dydx_na), sprintf("%.1f%%", std.error_na))

  flag <- mfx_class == "marginaleffects" &&
          tid_class == "data.frame" &&
          mfx_nrow > 0 &&
          tid_nrow > 0 &&
          dydx_unique >= n_unique &&
          dydx_na <= pct_na  &&
          (is.null(std.error_na) || std.error_unique >= n_unique) &&
          (is.null(std.error_na) || std.error_na <= pct_na)

  testthat::expect(isTRUE(flag), msg)

  invisible(act$val)
}
