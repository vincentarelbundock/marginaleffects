source("helpers.R")
using("marginaleffects")

# Golden values from Stata's `margins, vce(unconditional)`, the analogue of
# `vcovUnconditional()`. Regenerate with:
#   cd ~/nix-config/recipes/stata-se
#   make batch dofile=~/repos/marginaleffects/r/inst/tinytest/stata/unconditional.do
golden_path <- "stata/results/unconditional.csv"
if (!file.exists(golden_path)) exit_file("stata/results/unconditional.csv not generated")

golden <- read.csv(golden_path, stringsAsFactors = FALSE)

# The very same file `unconditional.do` reads, so the two sides cannot drift.
# Built by stata/databases/generate_iris.R; `wide`, `heavy` and `scount` are
# derived from the original measurements. The factor conversions below are the
# counterpart of Stata's `i.` prefix.
ir <- read.csv("stata/databases/iris.csv", stringsAsFactors = FALSE)
ir <- transform(
    ir,
    species = factor(species),
    heavy = factor(heavy),
    unit = factor(seq_len(nrow(ir)))
)

# `margins, vce(unconditional)` inherits the finite-sample multiplier of the
# estimation command's own robust VCE:
#
#   * `regress, vce(robust)` uses n / (n - k), which is `type = "HC1"`.
#   * the ML commands (logit, probit, poisson, glm) use n / (n - 1), which is
#     the G / (G - 1) cluster adjustment with one singleton cluster per
#     observation. Singleton clustering leaves the meat unchanged, so `cluster`
#     here supplies the multiplier and nothing else.
V_lm <- vcovUnconditional(type = "HC1")
V_ml <- vcovUnconditional(cluster = ~unit)

# `canonical` records whether the link is the canonical one for the family.
# Stata's ML commands report the OBSERVED information matrix while
# `sandwich::bread()` reports the EXPECTED (Fisher/IRLS) information, and the
# two coincide only for a canonical link. Rather than bend the R side to
# Stata's convention, `unconditional.do` estimates the two non-canonical fits
# with `glm ..., irls`, which puts Stata on the expected information as well.
# Both sides then compute the same estimator and the same bound applies to
# every model here. See PR #1737 (snhansen).
specs <- list(
    iris_lm_gaussian_identity = list(
        model = lm(swidth ~ slength + pwidth + heavy, data = ir),
        vcov = V_lm, by = "species", fac = "heavy", num = "slength", canonical = TRUE
    ),
    iris_glm_binomial_logit = list(
        model = glm(wide ~ slength + pwidth + heavy, family = binomial("logit"), data = ir),
        vcov = V_ml, by = "species", fac = "heavy", num = "slength", canonical = TRUE
    ),
    iris_glm_poisson_log = list(
        model = glm(scount ~ slength + pwidth + heavy, family = poisson("log"), data = ir),
        vcov = V_ml, by = "species", fac = "heavy", num = "slength", canonical = TRUE
    ),
    iris_glm_binomial_probit = list(
        model = glm(wide ~ slength + pwidth + heavy, family = binomial("probit"), data = ir),
        vcov = V_ml, by = "species", fac = "heavy", num = "slength", canonical = FALSE
    ),
    iris_glm_gamma_log = list(
        model = glm(swidth ~ slength + pwidth + heavy, family = Gamma("log"), data = ir),
        vcov = V_ml, by = "species", fac = "heavy", num = "slength", canonical = FALSE
    )
)

# Point estimates use no information matrix, so they agree for every model. The
# floor is Stata's own convergence tolerance, not R's.
tol_estimate <- 1e-4

# Slopes get their own bound: marginaleffects differentiates the estimand
# numerically while Stata does so analytically, which costs about one order of
# magnitude. This is finite-difference error, not a disagreement about the
# variance -- shrinking `eps` makes it worse, not better, because the default
# already sits near the cancellation optimum.
#
# The non-canonical fits carry the same bounds as the rest: aligning the
# information matrix in `unconditional.do` took them from 3-10% away to ~1e-5.
tol_std_error <- function(canonical, command) {
    if (startsWith(command, "avg_slopes")) 1e-4 else 1e-5
}

cmd <- function(spec, command) {
    m <- spec$model
    V <- spec$vcov
    switch(command,
        avg_predictions = avg_predictions(m, vcov = V),
        avg_predictions_by = avg_predictions(m, by = spec$by, vcov = V),
        avg_comparisons_fac = avg_comparisons(m, variables = spec$fac, vcov = V),
        avg_comparisons_fac_by = avg_comparisons(
            m,
            variables = spec$fac,
            by = spec$by,
            vcov = V
        ),
        # On a continuous variable Stata's `dydx()` is a derivative, so the
        # counterpart is avg_slopes(), not avg_comparisons(). On a factor the
        # two coincide, which is what `avg_comparisons_fac` above checks.
        avg_slopes_num = avg_slopes(m, variables = spec$num, vcov = V),
        avg_slopes_num_by = avg_slopes(m, variables = spec$num, by = spec$by, vcov = V)
    )
}

# `unconditional.do` exports through the shared mfx_* helpers, so a fixture is
# named "<model>_<command>" and the rows arrive in Stata's own order.
commands <- c(
    "avg_predictions",
    "avg_predictions_by",
    "avg_comparisons_fac",
    "avg_comparisons_fac_by",
    "avg_slopes_num",
    "avg_slopes_num_by"
)

tested <- character()
for (mod_name in names(specs)) {
    spec <- specs[[mod_name]]

    for (command in commands) {
        fixture <- paste(mod_name, command, sep = "_")
        sta <- golden[golden$fixture == fixture, , drop = FALSE]
        expect_true(nrow(sta) > 0, info = paste("no fixture", fixture))
        if (nrow(sta) == 0) next
        tested <- union(tested, fixture)

        mfx <- cmd(spec, command)
        # Stata's `over()` orders groups by the level ordering, same as `by=`.
        if (spec$by %in% colnames(mfx)) mfx <- mfx[order(mfx[[spec$by]]), ]

        label <- paste(mod_name, command, sep = ": ")
        expect_equal(nrow(mfx), nrow(sta), info = paste(label, "nrow"))
        expect_equal(
            mfx$estimate,
            sta$estimate,
            tolerance = tol_estimate,
            info = paste(label, "estimate")
        )
        expect_equal(
            mfx$std.error,
            sta$std_error,
            tolerance = tol_std_error(spec$canonical, command),
            info = paste(label, "std.error")
        )
    }
}

expect_equal(
    sort(unique(golden$fixture)),
    sort(tested),
    info = "every unconditional.csv fixture is tested"
)
