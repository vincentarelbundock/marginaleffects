multcomp_test <- function(
    object,
    multcomp = FALSE,
    conf_level = 0.95,
    df = Inf
) {
    valid <- c(
        "holm",
        "hochberg",
        "hommel",
        "bonferroni",
        "BH",
        "BY",
        "fdr",
        "single-step",
        "Shaffer",
        "Westfall",
        "free"
    )
    checkmate::assert(
        checkmate::check_choice(multcomp, choices = valid),
        checkmate::check_flag(multcomp)
    )

    if (isFALSE(multcomp)) {
        return(object)
    }

    if (isTRUE(multcomp)) multcomp <- "holm"

    insight::check_if_installed("multcomp")

    # `glht()` tests against 0, two-sided, unless told otherwise. Carry the
    # object's null (e.g. 1 for `ratio ~ ...`) and direction (`<=`, `>=`), so
    # the adjusted p values agree with the unadjusted `statistic` column.
    rhs <- 0
    alternative <- "two.sided"
    mfx <- attr(object, "marginaleffects")
    if (!is.null(mfx)) {
        if (isTRUE(checkmate::check_number(mfx@hypothesis_null))) {
            rhs <- mfx@hypothesis_null
        }
        direction <- unique(mfx@hypothesis_direction)
        if (length(direction) > 1) {
            stop_sprintf(
                "The `multcomp` argument requires all hypotheses to share the same direction (`=`, `<=`, or `>=`)."
            )
        }
        alternative <- switch(direction, "<=" = "greater", ">=" = "less", "two.sided")
    }

    # Explicit identity `linfct`: with `linfct` missing, the `glht()` generic
    # forwards `rhs` and `alternative` to `modelparm()` and then to `vcov()`.
    beta <- stats::coef(object)
    linfct <- diag(length(beta))
    rownames(linfct) <- names(beta)
    k <- multcomp::glht(object, linfct = linfct, rhs = rhs, alternative = alternative, df = df)
    k <- summary(k, test = multcomp::adjusted(type = multcomp))
    # Confidence intervals: `multcomp` only offers single-step (max-t)
    # simultaneous intervals. Bonferroni intervals are computed at the
    # Bonferroni-adjusted level so they agree with the Bonferroni p values;
    # the max-t family uses its own critical value; step-wise p value
    # adjustments (holm, hochberg, ...) have no matching interval and keep the
    # single-step intervals, which are conservative for them.
    if (identical(multcomp, "bonferroni")) {
        n_tests <- nrow(linfct)
        calpha <- stats::qt(1 - (1 - conf_level) / (2 * n_tests), df = if (is.finite(df)) df else Inf)
        k <- stats::confint(k, level = conf_level, calpha = calpha)
    } else {
        k <- stats::confint(k, level = conf_level)
    }
    object$p.value <- k$test$pvalues
    object$conf.low <- k$confint[, 2, drop = TRUE]
    object$conf.high <- k$confint[, 3, drop = TRUE]
    object$s.value <- -log2(object$p.value)

    return(object)
}
