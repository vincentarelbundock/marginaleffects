sanitize_hypothesis_formula <- function(hypothesis) {
  insight::check_if_installed("Formula")
  hypothesis <- Formula::Formula(hypothesis)
  if (!length(hypothesis)[1] %in% 0:1) {
    insight::format_error("The left-hand side of the `hypothesis` formula must be of length 0 or 1.")
  }
  if (!length(hypothesis)[2] %in% 1:2) {
    insight::format_error("The right-hand side of the `hypothesis` formula must have 1 or two parts, separated by |")
  }
  if (length(hypothesis)[1] == 0) {
    hypothesis <- stats::update(hypothesis, difference ~ .)
  }
  if (length(hypothesis)[2] == 1) {
    by <- NULL
  } else {
    by <- all.vars(stats::formula(hypothesis, lhs = 0, rhs = 2))
  }
  lhs <- all.vars(stats::formula(hypothesis, rhs = 0))
  rhs <- all.vars(stats::formula(hypothesis, lhs = 0, rhs = 1))

  checkmate::assert_choice(lhs, c("difference", "ratio"), .var.name = "left-hand side of `hypothesis` formula")
  checkmate::assert_choice(rhs, c("reference", "sequential", "meandev", "poly"), .var.name = "Right-hand side of `hypothesis` formula")

  out <- specify_hypothesis(
    hypothesis = rhs,
    comparison = lhs,
    by = by,
    internal = TRUE)

  return(out)
}
