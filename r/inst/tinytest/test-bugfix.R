source("helpers.R")
using("marginaleffects")


# Bug stay dead: Issue 55
# Error: Argument 1 must have names.
# vab: possibly caused by a version of `emmeans` < 1.6.3
dat <- get_dataset("penguins", "palmerpenguins")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, data = dat, family = binomial)
mfx <- slopes(mod, variables = "species")
expect_inherits(mfx, "data.frame")
expect_true(nrow(mfx) > 0)
expect_true(ncol(mfx) > 0)


# Hernan & Robins replication: bug would not detect `as.factor()` in formula()
nhefs <- get_dataset("nhefs", "causaldata")
f <- wt82_71 ~
    qsmk +
    sex +
    race +
    age +
    I(age * age) +
    factor(education) +
    smokeintensity +
    I(smokeintensity * smokeintensity) +
    smokeyrs +
    I(smokeyrs * smokeyrs) +
    as.factor(exercise) +
    as.factor(active) +
    wt71 +
    I(wt71 * wt71) +
    I(qsmk * smokeintensity)

fit <- glm(f, data = nhefs)
pre <- predictions(fit, newdata = nhefs)
mfx <- slopes(fit, newdata = nhefs)
cmp <- comparisons(fit, newdata = nhefs)
expect_inherits(pre, "predictions")
expect_inherits(cmp, "comparisons")
expect_inherits(mfx, "marginaleffects")


# Issue 372: reserved variable names
dat <- mtcars
dat$group <- dat$am
mod <- lm(mpg ~ group, data = dat)
expect_error(comparisons(mod), pattern = "forbidden")
mod <- lm(mpg ~ group + hp, data = dat)
expect_error(comparisons(mod), pattern = "forbidden")


# Issue #556
set.seed(12345)
n = 500
x = sample(1:3, n, replace = TRUE)
y = rnorm(n)
z = ifelse(x + y + rlogis(n) > 1.5, 1, 0)
dat = data.frame(x = factor(x), y = y, z = z)
dat <- dat

m1 = glm(z ~ x + y, family = binomial, data = dat)
nd <- datagrid(model = m1, y = seq(-2.5, 2.5, by = 0.25))
p1 <- predictions(m1, newdata = nd, type = "link")
p2 <- as.data.frame(predict(m1, newdata = nd, se.fit = TRUE))

expect_equal(p1$estimate, p2$fit, tolerance = 1e-6)
expect_equal(p1$std.error, p2$se.fit, tolerance = 1e-6)

set.seed(12345)
n = 60
x = sample(1:3, n, replace = TRUE)
z = ifelse(x + rlogis(n) > 1.5, 1, 0)
dat = data.frame(x = factor(x), z = z)
dat <- dat

m2 = glm(z ~ I(x == 2) + I(x == 3), family = binomial, data = dat)

p1 <- predictions(m2, type = "link")
p2 <- predictions(m2, newdata = dat, type = "link")
p3 <- as.data.frame(predict(m2, se.fit = TRUE, type = "link"))

expect_equal(p1$estimate, p3$fit, tolerance = 1e-6)
expect_equal(p1$std.error, p3$se.fit, tolerance = 1e-6)
expect_equal(p2$estimate, p3$fit, tolerance = 1e-6)
expect_equal(p2$std.error, p3$se.fit, tolerance = 1e-6)


# Issue #671
dta <- data.frame(
    lab = sample(0:1, size = 1000, replace = T),
    age_group = sample(c("old", "young"), size = 1000, replace = TRUE)
)
mod <- lm(lab ~ age_group, dta)
mfx <- avg_slopes(mod)
expect_equivalent(nrow(mfx), 1)
expect_true("young - old" %in% mfx$contrast)


# Issue #697
dat <- data.frame(
    outcome = rbinom(n = 100, size = 1, prob = 0.35),
    var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
    var_cont = rnorm(n = 100, mean = 10, sd = 7),
    group = sample(letters[1:4], size = 100, replace = TRUE),
    groups = sample(letters[1:4], size = 100, replace = TRUE)
)

m1 <- glm(
    outcome ~ var_binom + var_cont + group,
    data = dat,
    family = binomial()
)
expect_error(avg_slopes(m1), pattern = "forbidden")
expect_error(avg_slopes(m1, variables = "var_cont"), pattern = "forbidden")

dat$group <- NULL
m2 <- glm(
    outcome ~ var_binom + var_cont + groups,
    data = dat,
    family = binomial()
)
expect_inherits(avg_slopes(m2), "slopes")
expect_inherits(avg_slopes(m2, variables = "var_cont"), "slopes")

# Issue #723
dat <- data.frame(
    rbind(
        c(10., "A", "AU"),
        c(20., "A", "AU"),
        c(30., "A", "AU"),
        c(20., "B", "AU"),
        c(30., "B", "AU"),
        c(40., "B", "AU"),
        c(10., "B", "NZ"),
        c(20., "B", "NZ"),
        c(30., "B", "NZ"),
        c(20., "A", "NZ"),
        c(30., "A", "NZ"),
        c(40., "A", "NZ")
    )
)
colnames(dat) <- c("y", "treatment", "country")
mod <- lm(y ~ treatment * country, dat)
cmp <- comparisons(mod, variables = "treatment", by = "country")
expect_inherits(cmp, "comparisons")
expect_equivalent(nrow(cmp), 2)
expect_equivalent(
    cmp$estimate[cmp$country == "AU"],
    coef(mod)["treatmentB"]
)
expect_equivalent(
    cmp$estimate[cmp$country == "NZ"],
    coef(mod)["treatmentB"] + coef(mod)["treatmentB:countryNZ"]
)


# Issue #1005
d1 <- d2 <- mtcars
d2[["horse power"]] <- d2$hp
m1 <- lm(mpg ~ hp, data = d1)
m2 <- lm(mpg ~ `horse power`, data = d2)
p1 <- plot_predictions(m1, condition = "hp", draw = FALSE)
p2 <- plot_predictions(m2, condition = "horse power", draw = FALSE) |> suppressWarnings()
expect_equivalent(p1$estimate, p2$estimate)
expect_equivalent(p1$std.error, p2$std.error)


# Issue #1230
mod <- lm(mpg ~ 1, mtcars)
p <- avg_predictions(mod)
expect_false(is.na(p$estimate))
expect_error(avg_slopes(mod), "no valid predictor")
expect_error(avg_comparisons(mod), "no valid predictor")


# Issue #1660: avg_slopes() should not strip data.table class
dt <- data.table::data.table(x = rnorm(1000))
dt[, y := 4 + x + rnorm(1000)]
mod <- lm(data = dt, y ~ x)
avg_slopes(mod, variables = "x")
expect_true(data.table::is.data.table(dt))


# Issue #1744: warn when coefficients are NA (rank deficiency)
options(marginaleffects_safe = TRUE)
options(marginaleffects_rank_deficient = TRUE)
set.seed(42)
n <- 30
dat <- data.frame(
    arm = rep(c("control", "A", "A", "B", "B"), each = n),
    dose = rep(c("none", "low", "high", "low", "high"), each = n))
dat$y <- rnorm(nrow(dat))
dat$arm <- factor(dat$arm, levels = c("control", "A", "B"))
dat$dose <- factor(dat$dose, levels = c("none", "low", "high"))
mod <- lm(y ~ arm * dose, data = dat)
expect_warning(avg_predictions(mod, vcov = FALSE), pattern = "order of factor levels")
# once per session
expect_silent(avg_comparisons(mod, vcov = FALSE))
options(marginaleffects_rank_deficient = TRUE)
expect_warning(avg_comparisons(mod, vcov = FALSE), pattern = "order of factor levels")
options(marginaleffects_rank_deficient = TRUE)
options(marginaleffects_safe = FALSE)


# ---------------------------------------------------------------------------
# External review 2026-08: uncertainty machinery on the analytic Jacobian path
# ---------------------------------------------------------------------------

# Mixed-scale predictions: the comparison-stage derivative must stay exact
# when predictions span many orders of magnitude. A shared probe step scaled
# to the largest prediction used to overstep the smallest ones and corrupt
# ratio-type standard errors by orders of magnitude, silently.
set.seed(7)
n_span <- 300
dat_span <- data.frame(x = runif(n_span, -7, 7), z = rbinom(n_span, 1, 0.5))
dat_span$y <- rpois(n_span, exp(0.5 + 0.95 * dat_span$x + 0.3 * dat_span$z))
mod_span <- glm(y ~ x + z, data = dat_span, family = poisson)
for (cmp_fun in c("ratio", "lift", "lnratio")) {
    old <- options(marginaleffects_analytic_jacobian = TRUE)
    a <- comparisons(mod_span, variables = "z", comparison = cmp_fun)
    options(marginaleffects_analytic_jacobian = FALSE)
    b <- comparisons(mod_span, variables = "z", comparison = cmp_fun)
    options(old)
    expect_equal(components(a, "jacobian_method"), "analytic", info = cmp_fun)
    expect_equal(a$std.error, b$std.error, tolerance = 1e-5, info = cmp_fun)
}

# A custom comparison closure must fall back to the numeric path: probing
# arbitrary code cannot prove the structure the composition would rely on.
old <- options(marginaleffects_analytic_jacobian = TRUE)
cmp_custom <- comparisons(
    mod_span,
    variables = "z",
    comparison = function(hi, lo) hi - lo
)
options(old)
expect_equal(components(cmp_custom, "jacobian_method"), "numeric")

# User-supplied vcov matrices with permuted names are aligned everywhere:
# reported standard errors, the vcov() extractor, and stored slots.
mod_perm <- lm(mpg ~ hp + wt, data = mtcars)
V_perm <- vcov(mod_perm)
V_perm <- V_perm[rev(rownames(V_perm)), rev(colnames(V_perm))]
cmp_named <- comparisons(mod_perm, variables = c("hp", "wt"), newdata = "mean")
cmp_permuted <- comparisons(
    mod_perm,
    variables = c("hp", "wt"),
    newdata = "mean",
    vcov = V_perm
)
expect_equivalent(cmp_named$std.error, cmp_permuted$std.error)
expect_equivalent(diag(vcov(cmp_named)), diag(vcov(cmp_permuted)))
V_bad <- vcov(mod_perm)
dimnames(V_bad) <- list(c("a", "b", "c"), c("a", "b", "c"))
expect_error(
    comparisons(mod_perm, variables = "hp", newdata = "mean", vcov = V_bad),
    pattern = "names do not match"
)

# Simulation-based Wald tests subtract the stored null hypothesis value and
# never a hard-coded zero.
set.seed(1)
sim_got <- inferences(
    hypotheses(
        avg_comparisons(mod_perm, variables = "hp", comparison = "ratio"),
        hypothesis = "b1 = 1"
    ),
    method = "simulation",
    R = 200,
    conf_type = "wald"
)
sim_stored_null <- components(sim_got, "hypothesis_null")
if (!isTRUE(checkmate::check_number(sim_stored_null))) {
    sim_stored_null <- 0
}
expect_equivalent(
    sim_got$statistic,
    (sim_got$estimate - sim_stored_null) / sim_got$std.error
)
# A scalar numeric `hypothesis` stores a nonzero null; the simulation
# statistic must subtract it, not test against zero.
set.seed(1)
sim_null1 <- inferences(
    avg_comparisons(mod_perm, variables = "hp", comparison = "ratio", hypothesis = 1),
    method = "simulation",
    R = 200,
    conf_type = "wald"
)
expect_equivalent(
    sim_null1$statistic,
    (sim_null1$estimate - 1) / sim_null1$std.error
)

# A constant estimand has variance exactly zero, not NA.
cmp_zero <- comparisons(
    mod_perm,
    variables = c("hp", "wt"),
    newdata = "mean",
    hypothesis = matrix(c(0, 0), ncol = 1)
)
expect_equivalent(cmp_zero$std.error, 0)

# Stata's vce(robust) is HC1.
expect_equivalent(
    marginaleffects:::get_vcov(mod_perm, vcov = "stata"),
    sandwich::vcovHC(mod_perm, type = "HC1")
)

# Formula offsets are detected for models which store no $offset component.
if (requireNamespace("quantreg", quietly = TRUE)) {
    mod_rq_offset <- quantreg::rq(mpg ~ hp + offset(wt), data = mtcars, tau = 0.5)
    expect_true(marginaleffects:::model_has_effective_offset(mod_rq_offset))
    mod_rq_plain <- quantreg::rq(mpg ~ hp, data = mtcars, tau = 0.5)
    expect_false(marginaleffects:::model_has_effective_offset(mod_rq_plain))
}

# Richardson accepts its real `side` argument.
expect_silent(
    comparisons(
        mod_perm,
        variables = "hp",
        newdata = "mean",
        numderiv = list("richardson", side = 1)
    )
)

# A joint-test name matching zero or several estimates errors instead of
# silently building a wrong restriction row. (These labels match nothing:
# the comparison names carry contrast suffixes.)
cmp_joint <- comparisons(mod_perm, variables = c("hp", "wt"), newdata = "mean")
expect_error(
    hypotheses(cmp_joint, joint = c("hp", "hp")),
    pattern = "exactly one"
)


# Issue #1748: `stats::predict()` fails when `qr$qr` is deleted from the fit to
# save memory. Linear predictions do not need a QR decomposition, so the model
# matrix path produces the same estimates.
options(marginaleffects_rank_deficient = TRUE)
dat <- mtcars
dat$hp2 <- dat$hp + 2
mod <- lm(mpg ~ hp + hp2, data = dat)
stripped <- mod
stripped$qr$qr <- NULL
for (FUN in list(avg_predictions, avg_comparisons, avg_slopes)) {
    known <- suppressWarnings(FUN(mod, vcov = FALSE))
    unknown <- suppressWarnings(FUN(stripped, vcov = FALSE))
    expect_equivalent(known$estimate, unknown$estimate)
}

mod <- glm(am ~ hp + hp2, data = dat, family = binomial())
stripped <- mod
stripped$qr$qr <- NULL
for (ty in c("response", "link")) {
    known <- suppressWarnings(avg_predictions(mod, type = ty, vcov = FALSE))
    unknown <- suppressWarnings(avg_predictions(stripped, type = ty, vcov = FALSE))
    expect_equivalent(known$estimate, unknown$estimate)
}
known <- suppressWarnings(avg_comparisons(mod, vcov = FALSE))
unknown <- suppressWarnings(avg_comparisons(stripped, vcov = FALSE))
expect_equivalent(known$estimate, unknown$estimate)

# unrelated `predict()` failures still report their own cause
expect_error(
    predictions(mod, newdata = data.frame(zzz = 1)),
    pattern = "object 'hp' not found"
)
options(marginaleffects_rank_deficient = TRUE)

# get_coef() must label the precision parameter the way get_vcov() does, so a
# user-supplied variance-covariance matrix is accepted. betareg names the lone
# precision coefficient "(phi)" when the model has no precision formula, and
# prefixing it again produced "(phi)_(phi)".
requiet("betareg")
data("GasolineYield", package = "betareg")
mod <- betareg::betareg(yield ~ batch + temp, data = GasolineYield)
expect_equal(names(get_coef(mod)), colnames(get_vcov(mod)))
expect_inherits(
    avg_slopes(mod, variables = "temp", vcov = stats::vcov(mod)),
    "slopes"
)
# a precision formula gives the precision coefficients ordinary names, and
# those must still be prefixed
mod <- betareg::betareg(yield ~ batch + temp | temp, data = GasolineYield)
expect_equal(names(get_coef(mod)), colnames(get_vcov(mod)))


# Zero or missing weights poisoned the redundant re-aggregation of comparison
# estimates the `*avgwts` functions had already collapsed: each (term,
# contrast) group survived as a single row carrying the stale unit-level
# weight of its first source row, and re-averaging that lone row by a zero
# weight is 0/0 = NaN. Zero weights are routine (ATT/matching), and the
# failure depended on which row happened to sort first.
set.seed(1024)
n <- 40
dat <- data.frame(
    x = rnorm(n),
    f = factor(rep(c("u", "v"), n / 2))
)
dat$y <- rbinom(n, 1, plogis(dat$x))
dat$w <- c(0, runif(n - 1, .5, 1)) # zero weight on the first row
mod <- glm(y ~ x + f, family = binomial, data = dat)
cmp <- avg_comparisons(mod, wts = dat$w)
expect_false(anyNA(cmp$estimate))
expect_false(anyNA(cmp$std.error))
# the aggregate must equal the weighted mean of the unit-level comparisons
uni <- comparisons(mod, wts = dat$w)
for (i in seq_len(nrow(cmp))) {
    idx <- uni$term == cmp$term[i] & uni$contrast == cmp$contrast[i]
    expect_equivalent(
        cmp$estimate[i],
        stats::weighted.mean(uni$estimate[idx], dat$w[uni$rowid][idx])
    )
}


# An affine hypothesis -- linear plus a constant -- was not promoted to an
# exact contrast matrix, so its derivative came from a finite-difference probe
# through the hypothesis arithmetic. A large additive constant cancels the
# probe catastrophically: "b1 + 1e16 = 0" reported SE = 0 with full
# confidence. The derivative of an affine map does not depend on the
# constant, so the SE must equal the unhypothesized one exactly.
mod <- lm(mpg ~ hp + wt, mtcars)
for (analytic in c(TRUE, FALSE)) {
    options(marginaleffects_analytic_jacobian = analytic)
    base <- predictions(mod, newdata = "mean")
    aff <- predictions(mod, newdata = "mean", hypothesis = "b1 + 1e16 = 0")
    expect_equivalent(aff$estimate, base$estimate + 1e16)
    expect_equivalent(aff$std.error, base$std.error, tolerance = 1e-10)
    aff2 <- predictions(mod, newdata = "mean", hypothesis = "b1 + 1e8 = 0")
    expect_equivalent(aff2$std.error, base$std.error, tolerance = 1e-10)
    # nonzero right-hand side is the same affine map
    aff3 <- predictions(mod, newdata = "mean", hypothesis = "b1 = 5")
    expect_equivalent(aff3$estimate, base$estimate - 5)
    expect_equivalent(aff3$std.error, base$std.error, tolerance = 1e-10)
}
options(marginaleffects_analytic_jacobian = TRUE)


# Policy: a genuine singleton group whose only member has zero weight is an
# undefined weighted mean and must stay NaN. Only rows the comparison stage
# already collapsed carry a group-total weight into further aggregation.
d0 <- data.frame(y = c(1, 2), x = c(0, 1), g = c("a", "b"), w = c(0, 1))
m0 <- lm(y ~ x, d0)
p0 <- avg_predictions(m0, newdata = d0, by = "g", wts = "w", vcov = FALSE)
expect_true(is.nan(p0$estimate[p0$g == "a"]))
expect_equivalent(p0$estimate[p0$g == "b"], 2)


# The centering hypothesis shortcuts must not materialize their dense n x n
# operators; their structured pullback must equal the dense algebra exactly,
# and end-to-end standard errors must agree with the numeric path.
set.seed(11)
J <- matrix(rnorm(21), nrow = 7)
groups <- list(1:3, 4:7)
dense_block <- function(shortcut, n) {
    if (shortcut == "meandev") {
        diag(n) - matrix(1 / n, n, n)
    } else {
        H <- diag(n) - matrix(1 / (n - 1), n, n)
        diag(H) <- 1
        H
    }
}
for (shortcut in c("meandev", "meanotherdev")) {
    pb <- marginaleffects:::hypothesis_formula_pullback(shortcut, groups)
    expected <- rbind(
        t(dense_block(shortcut, 3)) %*% J[1:3, , drop = FALSE],
        t(dense_block(shortcut, 4)) %*% J[4:7, , drop = FALSE]
    )
    expect_equivalent(pb(J), expected)
}
dat <- transform(mtcars, cyl = factor(cyl))
mod <- glm(am ~ mpg + cyl, family = binomial, data = dat)
for (shortcut in list(~meandev, ~meanotherdev)) {
    a <- avg_predictions(mod, by = "cyl", hypothesis = shortcut)
    expect_equal(components(a, "jacobian_method"), "analytic")
    options(marginaleffects_analytic_jacobian = FALSE)
    b <- avg_predictions(mod, by = "cyl", hypothesis = shortcut)
    options(marginaleffects_analytic_jacobian = TRUE)
    expect_equivalent(a$estimate, b$estimate)
    expect_equivalent(a$std.error, b$std.error, tolerance = 1e-6)
}


# Deep review (2026-09): user-supplied `vcov` matrix covering only a subset
# of the coefficients must error instead of treating the missing ones as constants
mod <- glm(am ~ hp + wt, mtcars, family = binomial)
expect_error(
    avg_comparisons(mod, variables = "hp", vcov = vcov(mod)[1:2, 1:2]),
    pattern = "coefficient"
)
# a `vcov` function that raises an error must not silently fall back to the default
expect_error(
    avg_comparisons(mod, variables = "hp", vcov = function(m) stop("boom")),
    pattern = "boom"
)

# Deep review (2026-09): `by=` merge must join on row identifiers, not on
# internal column names such as `predicted`
dat <- transform(mtcars, cyl = factor(cyl), gear = factor(gear))
dat$predicted <- fitted(lm(mpg ~ hp * cyl + wt, dat))
mod <- lm(mpg ~ hp * cyl + wt, dat)
cmp <- avg_comparisons(mod, variables = "hp", by = "cyl", vcov = FALSE)
manual <- tapply(comparisons(mod, variables = "hp", vcov = FALSE)$estimate, dat$cyl, mean)
expect_equal(nrow(cmp), 3)
expect_equivalent(cmp$estimate, as.numeric(manual))

# Deep review (2026-09): `"all"` contrasts must not depend on which
# categorical variable happens to come first in the model
mod <- lm(mpg ~ hp + cyl + gear, dat)
expect_equal(nrow(avg_comparisons(mod, variables = list(cyl = "all"), vcov = FALSE)), 9)
expect_equal(nrow(avg_comparisons(mod, variables = list(gear = "all"), vcov = FALSE)), 9)

# Deep review (2026-09): `multcomp` must respect the hypothesis null and direction
if (requiet("multcomp")) {
    mod <- lm(mpg ~ hp + wt + factor(cyl), mtcars)
    p <- avg_predictions(mod, by = "gear")
    r1 <- hypotheses(p, hypothesis = ratio ~ pairwise)
    r2 <- hypotheses(p, hypothesis = ratio ~ pairwise, multcomp = "holm")
    expect_equivalent(r2$p.value, p.adjust(r1$p.value, "holm"), tolerance = 1e-6)
    h1 <- hypotheses(mod, hypothesis = c("hp <= 0", "wt <= 0"))
    h2 <- hypotheses(mod, hypothesis = c("hp <= 0", "wt <= 0"), multcomp = "holm")
    expect_equivalent(h2$p.value, p.adjust(h1$p.value, "holm"), tolerance = 1e-6)
}

# Deep review (2026-09): `factor(x, levels = ...)` in a formula must be
# recognized as an in-formula factor so `datagrid()` fills it with a level
set.seed(4)
dd <- data.frame(y = rnorm(40), z = rnorm(40), x = sample(c(1L, 10L), 40, TRUE))
mod <- lm(y ~ z + factor(x, levels = c(10, 1)), dd)
expect_true(datagrid(model = mod, z = 0)$x %in% c(1, 10))
expect_false(anyNA(predictions(mod, newdata = datagrid(z = 0))$estimate))

# Deep review (2026-09): user-supplied `newdata` must not be converted to a
# data.table by reference (happened when it already had a `rowid` column)
mod <- lm(mpg ~ hp + wt, mtcars)
nd <- datagrid(model = mod, hp = 1:3)
invisible(predictions(mod, newdata = nd))
expect_false(inherits(nd, "data.table"))
p <- predictions(mod, newdata = datagrid(hp = c(100, 150)))
invisible(predictions(mod, newdata = p))
expect_inherits(p, "predictions")

# Deep review (2026-09): bootstrap refit with a `weights=` vector that is
# not a column of the data would silently pair resampled rows with the
# original weights; must error clearly. Column weights keep working.
wvec <- mtcars$carb
mod <- lm(mpg ~ hp + wt, data = mtcars, weights = wvec)
expect_error(
    inferences(avg_predictions(mod, by = "cyl"), method = "boot", R = 5),
    pattern = "weights"
)
mod <- lm(mpg ~ hp + wt, data = mtcars, weights = carb)
expect_inherits(
    inferences(avg_predictions(mod, by = "cyl"), method = "boot", R = 5),
    "predictions"
)

# Deep review (2026-09): split conformal half-width must be the
# ceiling((n + 1) * conf_level)-th smallest calibration score, and the
# score must use the transformed response when the formula transforms it
set.seed(1)
dtr <- data.frame(x = rnorm(200))
dtr$y <- 1 + 2 * dtr$x + rnorm(200)
dtr$w <- exp(dtr$y)
dcal <- data.frame(x = rnorm(22))
dcal$y <- 1 + 2 * dcal$x + rnorm(22)
dcal$w <- exp(dcal$y)
mod <- lm(y ~ x, dtr)
o <- inferences(
    predictions(mod, newdata = dtr[1:5, ], conf_level = 0.9),
    method = "conformal_split",
    data_calib = dcal
)
d <- sort(abs(dcal$y - predict(mod, newdata = dcal)))[ceiling(23 * 0.9)]
expect_equivalent(o$pred.high - o$estimate, rep(d, 5))
expect_equivalent(o$estimate - o$pred.low, rep(d, 5))
modlog <- lm(log(w) ~ x, dtr)
o2 <- inferences(
    predictions(modlog, newdata = dtr[1:5, ], conf_level = 0.9),
    method = "conformal_split",
    data_calib = dcal
)
expect_equivalent(o2$pred.high - o2$estimate, rep(d, 5))


# Deep review (2026-09), batch 2

# numeric `variables=` data frame / function: honour `lo`/`hi` column names
mod <- lm(mpg ~ hp + wt + factor(cyl), mtcars)
expected <- unname(10 * coef(mod)["hp"])
a <- avg_comparisons(mod, variables = list(hp = data.frame(hi = mtcars$hp + 10, lo = mtcars$hp)), vcov = FALSE)
b <- avg_comparisons(mod, variables = list(hp = function(x) data.frame(high = x + 10, low = x)), vcov = FALSE)
d <- avg_comparisons(mod, variables = list(hp = function(x) data.table::data.table(lo = x, hi = x + 10)), vcov = FALSE)
expect_equivalent(a$estimate, expected)
expect_equivalent(b$estimate, expected)
expect_equivalent(d$estimate, expected)

# logical variables: honour user-supplied contrast specifications
dat <- transform(mtcars, am = as.logical(am))
mod <- lm(mpg ~ hp + am, dat)
ref <- avg_comparisons(mod, variables = "am", vcov = FALSE)$estimate
expect_equivalent(avg_comparisons(mod, variables = list(am = c(FALSE, TRUE)), vcov = FALSE)$estimate, ref)
expect_equivalent(avg_comparisons(mod, variables = list(am = c(TRUE, FALSE)), vcov = FALSE)$estimate, -ref)
expect_equivalent(avg_comparisons(mod, variables = list(am = "revreference"), vcov = FALSE)$estimate, -ref)

# counterfactual datagrid with `by=`: rowidcf identifies the original row
mod <- lm(mpg ~ hp + wt, mtcars)
d <- datagrid(model = mod, hp = c(1, 2), by = "cyl", grid_type = "counterfactual")
expect_equal(nrow(d), 64)
expect_equal(length(unique(d$rowidcf)), 32)

# fwb: refit on modeldata, so NA rows and function-local data work
if (requiet("fwb")) {
    dat <- mtcars
    dat$hp[c(1, 5)] <- NA
    mna <- lm(mpg ~ hp + wt, data = dat)
    expect_inherits(inferences(avg_predictions(mna, by = "cyl"), method = "fwb", R = 10), "predictions")
    fit_in_fun <- function() {
        dd <- mtcars
        lm(mpg ~ hp + wt, data = dd)
    }
    expect_inherits(inferences(avg_predictions(fit_in_fun(), by = "cyl"), method = "fwb", R = 10), "predictions")
}

# backtransform: NaN bounds must not crash; bad transform output must error
mod <- lm(mpg ~ hp + wt, mtcars)
cmp <- suppressWarnings(avg_comparisons(mod, variables = "wt", transform = log))
expect_true(is.nan(cmp$conf.low) || is.na(cmp$conf.low))
expect_error(predictions(mod, newdata = head(mtcars, 3), transform = function(x) x[1]), pattern = "length")

# plot: fivenum labels which collide at 2 digits must not crash
set.seed(1)
dd <- data.frame(y = rnorm(100), x = rnorm(100), z = 10 + runif(100) / 10)
mod <- lm(y ~ x * z, dd)
p <- plot_predictions(mod, condition = c("x", "z"), draw = FALSE)
expect_equal(length(unique(p$z)), 5)

# newdata = "median" with a logical predictor
set.seed(3)
dat <- data.frame(y = rnorm(40), x = rnorm(40), lg = sample(c(TRUE, FALSE), 40, TRUE))
mod <- lm(y ~ x + lg, dat)
expect_inherits(predictions(mod, newdata = "median"), "predictions")

# mice: newdata = datagrid() works with `mira` objects
if (requiet("mice")) {
    dat <- mtcars
    dat$hp[c(2, 7)] <- NA
    imp <- mice::mice(dat, m = 3, printFlag = FALSE, seed = 1)
    fit <- with(imp, lm(mpg ~ hp + wt))
    p <- predictions(fit, newdata = datagrid(hp = c(100, 200)))
    expect_equal(nrow(p), 2)
}

# datagrid(response=) is honoured
mod <- lm(mpg ~ hp + wt, mtcars)
expect_false("mpg" %in% names(datagrid(model = mod, hp = 1, response = FALSE)))
expect_true("mpg" %in% names(datagrid(model = mod, hp = 1, response = TRUE)))
expect_true("mpg" %in% names(datagrid(model = mod, hp = 1, response = TRUE, grid_type = "balanced")))

# unnamed user vcov matrix: warn, since it is multiplied positionally
mod <- glm(am ~ hp + wt, mtcars, family = binomial)
options(marginaleffects_safe = TRUE)
expect_warning(avg_comparisons(mod, variables = "hp", vcov = unname(vcov(mod))), pattern = "names")
options(marginaleffects_safe = FALSE)

# df vector is rejected when estimates are aggregated
mod <- lm(mpg ~ hp + wt, mtcars)
expect_error(avg_predictions(mod, df = seq_len(32)), pattern = "df")
expect_error(predictions(mod, newdata = mtcars[c(1, 3, 5), ], by = "cyl", df = c(1, 100, 1000)), pattern = "df")
# df = NULL is accepted
expect_inherits(avg_predictions(mod, df = NULL), "predictions")

# joint: single string tries an exact match before regex; no match errors clearly
mod <- lm(mpg ~ hp + wt + factor(cyl), mtcars)
j <- hypotheses(mod, joint = "factor(cyl)6")
expect_equal(j$df1, 1)
expect_error(hypotheses(mod, joint = "nonexistent_term"), pattern = "match")

# equivalence bounds must be ordered
expect_error(hypotheses(mod, hypothesis = "wt = 0", equivalence = c(-2, -4)), pattern = "equivalence")

# hypothesis function returning nothing must error, not return 0 rows
expect_error(hypotheses(mod, hypothesis = function(x) x$estimate[1] / x$estimate[2]), pattern = "hypothesis")

# multcomp = "bonferroni" reports Bonferroni confidence intervals
if (requiet("multcomp")) {
    h <- hypotheses(mod, multcomp = "bonferroni")
    k <- nrow(h)
    q <- qnorm(1 - 0.025 / k)
    expect_equivalent(h$conf.low, h$estimate - q * h$std.error, tolerance = 1e-6)
    expect_equivalent(h$conf.high, h$estimate + q * h$std.error, tolerance = 1e-6)
}

# print(style = "tinytable") keeps Term/Comparison information as notes
if (requiet("tinytable")) {
    x <- comparisons(lm(mpg ~ hp * wt, mtcars), variables = "hp")
    tab <- print(x, style = "tinytable")
    notes <- unlist(tab@notes)
    expect_true(any(grepl("Term: hp", notes)))
    expect_true(any(grepl("Comparison", notes)))
}

# points= / rug= with a cbind() response
set.seed(2)
dd3 <- data.frame(s = rbinom(50, 10, .5), x = rnorm(50))
dd3$f <- 10 - dd3$s
mb <- glm(cbind(s, f) ~ x, family = binomial, dd3)
p <- plot_predictions(mb, condition = "x", points = 0.5, rug = TRUE)
expect_inherits(ggplot2::ggplot_build(p), "ggplot_built")

# get_mode ignores NA; datagrid(by=) warns on NA groups; tukey/grid expand integer columns
nd <- data.frame(y = rnorm(10), g = c(rep(NA, 4), rep("a", 3), rep("b", 3)), x = rnorm(10))
expect_true(datagrid(newdata = nd)$g %in% c("a", "b"))
nd <- mtcars
nd$cyl[1:3] <- NA
options(marginaleffects_safe = TRUE)
expect_warning(datagrid(newdata = nd, by = "cyl"), pattern = "missing")
options(marginaleffects_safe = FALSE)
set.seed(3)
dat <- data.frame(y = rnorm(40), x = rnorm(40), hp = sample(1:4, 40, TRUE), bin = sample(0:1, 40, TRUE))
mod <- lm(y ~ x + hp + bin, dat)
g <- predictions(mod, newdata = "grid")
expect_true(length(unique(g$hp)) > 1)
expect_equal(length(unique(g$bin)), 2)
t <- predictions(mod, newdata = "tukey")
expect_true(length(unique(t$hp)) > 1)

# expect_margins() must not pass vacuously on NA references
mod <- glm(am ~ hp + wt, family = binomial, data = mtcars)
res <- slopes(mod, variables = "hp")
fake <- data.frame(dydx_hp = rep(NA_real_, 32), SE_dydx_hp = NA_real_)
flag <- isTRUE(as.logical(marginaleffects:::expect_margins(res, fake, se = TRUE)))
expect_false(flag)
