
get_eti <- function(object, credMass = 0.95, ...) {
  result <- c(NA_real_, NA_real_)
  checkmate::assert_numeric(object)
  checkmate::assert_numeric(credMass, len = 1)
  checkmate::assert_true(credMass > 0)
  checkmate::assert_true(credMass < 1)
  critical <- (1 - credMass) / 2
  out <- stats::quantile(object, probs = c(critical, 1 - critical))
  out <- stats::setNames(out, c("lower", "upper"))
  return(out)
}
