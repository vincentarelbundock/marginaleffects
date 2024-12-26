multcomp_test <- function(object, multcomp = FALSE, conf_level = 0.95) {
  valid <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "single-step", "Shaffer", "Westfall", "free")
  checkmate::assert(
    checkmate::check_choice(multcomp, choices = valid),
    checkmate::check_flag(multcomp)
  )

  if (isFALSE(multcomp)) {
    return(object)
  }

  if (isTRUE(multcomp)) multcomp <- "holm"

  insight::check_if_installed("multcomp")

  k <- multcomp::glht(object)
  k <- summary(k, test = multcomp::adjusted(type = multcomp))
  k <- stats::confint(k, level = conf_level)
  object$p.value <- k$test$pvalues
  object$conf.low <- k$confint[, 2, drop = TRUE]
  object$conf.high <- k$confint[, 3, drop = TRUE]
  object$s.value <- -log2(object$p.value)

  return(object)
}

