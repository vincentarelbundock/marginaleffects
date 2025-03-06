sanitize_hypothesis_formula <- function(hypothesis) {
  insight::check_if_installed("Formula")
  hypothesis <- Formula::Formula(hypothesis)

  if (!length(hypothesis)[1] %in% 0:1) {
    stop("The left-hand side of the `hypothesis` formula must be of length 0 or 1.", call. = FALSE)
  }

  if (!length(hypothesis)[2] %in% 1:2) {
    stop("The right-hand side of the `hypothesis` formula must have 1 or two parts, separated by |", call. = FALSE)
  }

  if (length(hypothesis)[2] == 1) {
    by <- NULL
  } else {
    by <- all.vars(stats::formula(hypothesis, lhs = 0, rhs = 2))
  }
  lhs <- all.vars(stats::formula(hypothesis, rhs = 0))
  rhs <- all.vars(stats::formula(hypothesis, lhs = 0, rhs = 1))

  # Custom functions
  if (identical(rhs, "x")) {
    asis <- attr(stats::terms(hypothesis), "term.labels")
    asis <- grep("^I\\(", asis, value = TRUE)
    if (length(asis) == 1) {
      if (length(lhs) != 0) {
        stop("The left-hand side of `hypothesis` must be empty when using custom functions.", call. = FALSE)
      }
      asis <- sub("^I\\((.*)\\)$", "\\1", asis)
      out <- list(
        lhs = "arbitrary_function",
        rhs = asis,
        group = by
      )
      return(out)
    } else if (length(asis) > 1) {
      stop("Only one custom function is supported at a time on the right-hand side of the `hypothesis` formula.", call. = FALSE)
    }
  }

  valid <- c("reference", "revreference", "sequential", "pairwise", "revpairwise", "meandev", "meanotherdev", "poly", "helmert", "trt_vs_ctrl")
  checkmate::assert_choice(rhs, valid, .var.name = "Right-hand side of `hypothesis` formula")

  # dot product weights
  if (rhs %in% c("poly", "helmert")) {
    if (length(lhs) == 1 && lhs != "dotproduct") {
      stop("The left-hand size of the `hypothesis` formula must be empty or `dotproduct`.", call. = FALSE)
    } else {
      lhs <- "dotproduct"
    }

    # differences and ratios
  } else {
    if (length(lhs) == 1 && !lhs %in% c("difference", "ratio")) {
      stop("The left-hand size of the `hypothesis` formula must be empty, `difference`, or `ratio`.", call. = FALSE)
    } else if (length(lhs) == 0) {
      lhs <- "difference"
    }
  }

  out <- list(
    rhs = rhs,
    lhs = lhs,
    group = by
  )

  return(out)
}
