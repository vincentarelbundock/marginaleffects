multcomp_test <- function(object, multcomp = FALSE, conf_level = 0.95) {
    valid <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "single-step", "Shaffer", "Westfall", "free")
    checkmate::assert(
        checkmate::check_choice(multcomp, choices = valid),
        checkmate::check_flag(multcomp)
    )

    if (isFALSE(multcomp)) return(object)

    if (isTRUE(multcomp)) multcomp <- "holm"

    insight::check_if_installed("multcomp")

    s <- multcomp::glht(object) 
    s <- summary(s, test = multcomp::adjusted(type = multcomp))
    s <- stats::confint(s, level = conf_level)
    object$p.value <- s$test$pvalues
    object$conf.low <- s$confint[, 2, drop = TRUE]
    object$conf.high <- s$confint[, 3, drop = TRUE]

    return(object)
}