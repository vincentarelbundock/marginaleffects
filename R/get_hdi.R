get_hdi <- function(object, credMass = 0.95, ...) {
  result <- c(NA_real_, NA_real_)
  if(is.numeric(object)) {
    attributes(object) <- NULL
    x <- sort.int(object, method='quick')  # removes NA/NaN, but not Inf
    n <- length(x)
    if(n > 0) {
      # exclude <- ceiling(n * (1 - credMass)) # Not always the same as...
      exclude <- n - floor(n * credMass)       # Number of values to exclude
      low.poss <- x[1:exclude]             # Possible lower limits...
      upp.poss <- x[(n - exclude + 1):n]   # ... and corresponding upper limits
      best <- which.min(upp.poss - low.poss)      # Combination giving the narrowest interval
      if(length(best)) {
        result <- c(low.poss[best], upp.poss[best])
      } else {
        tmp <- range(x)
        if(length(tmp) == 2)
          result <- tmp
      }
    }
  }
  names(result) <- c("lower", "upper")
  return(result)
}