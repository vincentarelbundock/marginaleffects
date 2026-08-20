source("helpers.R")
using("marginaleffects")
requiet("survival")
requiet("emmeans")
requiet("broom")
requiet("splines")

# Basic expectation tests
mod_simple <- survival::coxph(Surv(time, status) ~ age + sex, data = lung)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# Issue #911: survreg support
fit <- survreg(Surv(futime, fustat) ~ ecog.ps + rx, ovarian, dist = "weibull", scale = 1)
s <- avg_slopes(fit)
expect_inherits(s, "slopes")


# clogit
N <- 10000
ng <- 5000
exd <- data.frame(
    g = rep(1:ng, each = N / ng),
    out = rep(0L:1L, N / 2),
    x = sample(0L:1L, N / 2, prob = c(.8, .2), replace = TRUE)
)
mod <- clogit(
    out ~ x + strata(g),
    method = "exact",
    data = exd
)

mfx <- slopes(mod, type = "lp")
expect_inherits(mfx, "marginaleffects")
cmp <- comparisons(mod, type = "lp")
expect_inherits(cmp, "comparisons")
pre <- predictions(mod, type = "lp")
expect_inherits(pre, "predictions")


# coxph vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$survival_coxph_01
test1 <<- data.frame(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    sex = factor(c(0, 0, 0, 0, 1, 1, 1))
)
mod <- coxph(Surv(time, status) ~ x + strata(sex), data = test1, ties = "breslow")
mfx <- merge(avg_slopes(mod, type = "lp"), stata)
expect_slopes(mod, type = "risk", n_unique = 4)
expect_equivalent(mfx$estimate, mfx$dydxstata)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = 1e-5)

# emtrends
em <- emtrends(mod, ~x, "x", at = list(time = 4, status = 1, x = 0, sex = factor(0, levels = 0:1)))
em <- tidy(em)
mfx <- slopes(mod, variables = "x", type = "lp")
expect_equivalent(mfx$estimate[1], em$x.trend)
expect_equivalent(mfx$std.error[1], em$std.error, tolerance = 1e-5)


# coxph: no validity
test2 <<- data.frame(
    start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
    stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
    event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
    x = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)
)
mod <- coxph(Surv(start, stop, event) ~ x, test2)
expect_slopes(mod, type = "risk", n_unique = 2)


# bugs stay dead: conf.level forces get_predicted which doesn't process 'type'
test3 <<- data.frame(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    sex = factor(c(0, 0, 0, 0, 1, 1, 1))
)
mod <- coxph(Surv(time, status) ~ x + strata(sex), data = test3, ties = "breslow")
p1 <- predictions(mod, type = "lp")
p2 <- predictions(mod, type = "risk")
expect_true(all(p1$estimate != p2$estimate))


# bugs stay dead: numeric vs factor strata
# skip_if_not_installed("insight", minimum_version = "0.17.0")
stata <- readRDS(testing_path("stata/stata.rds"))$survival_coxph_01
test4 <<- data.frame(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    sex = factor(c(0, 0, 0, 0, 1, 1, 1))
)
test5 <<- data.frame(
    time = c(4, 3, 1, 1, 2, 2, 3),
    status = c(1, 1, 1, 0, 1, 1, 0),
    x = c(0, 2, 1, 1, 1, 0, 0),
    sex = c(0, 0, 0, 0, 1, 1, 1)
)
mod1 <- coxph(Surv(time, status) ~ x + strata(sex), data = test4, ties = "breslow")
mod2 <- coxph(Surv(time, status) ~ x + strata(sex), data = test5, ties = "breslow")

mfx1 <- merge(avg_slopes(mod1, type = "lp"), stata)
mfx2 <- merge(avg_slopes(mod2, type = "lp"), stata)
expect_equivalent(mfx1$estimate, mfx2$estimate)


# Issue #1079
set.seed(12345)
aml <- survival::aml |>
    transform(z = rnorm(nrow(aml), 0, 1)) |>
    transform(zcat = cut(z, breaks = c(-10, -0.5, 0.5, 10)))
mod <- coxph(Surv(time, status == 1) ~ x * zcat, data = aml)
nd_n <- datagrid(model = mod, x = "Nonmaintained", zcat = unique)
nd_m <- datagrid(model = mod, x = "Maintained", zcat = unique)
p_n <- predict(mod, newdata = nd_n, type = "lp")
p_m <- predict(mod, newdata = nd_m, type = "lp")
e0 <- transform(nd_n, estimate = p_n - p_m)
e1 <- comparisons(mod, variables = "x", newdata = datagrid(zcat = unique), type = "lp")
e2 <- plot_comparisons(mod, variables = "x", condition = "zcat", type = "lp", draw = FALSE)
e3 <- hypotheses(
    mod,
    hypothesis = c(
        "xNonmaintained = 0",
        "xNonmaintained + `xNonmaintained:zcat(-0.5,0.5]` = 0",
        "xNonmaintained + `xNonmaintained:zcat(0.5,10]` = 0"
    )
)
expect_equivalent(e0$estimate, e1$estimate)
expect_equivalent(e0$estimate, e2$estimate)
expect_equivalent(e0$estimate, e3$estimate)


# Issue #1272
fit <- survreg(Surv(time, status) ~ ph.ecog + age + sex, lung, dist = "weibull")
p1 <- avg_predictions(fit, variables = "sex", type = "quantile", p = 0.5)
p2 <- avg_predictions(fit, variables = "sex", type = "quantile", p = 0.1)
expect_true(all(p1$estimate > p2$estimate))


# Issue #1467: warning about anti-conservative standard errors
op <- getOption("marginaleffects_safe")
options(marginaleffects_safe = TRUE)
dat <- survival::rotterdam
mod <- coxph(
    Surv(dtime, death) ~ hormon * factor(grade) + ns(age, df = 2),
    data = dat
)
expect_warning(avg_comparisons(mod), pattern = "bootstrap")
expect_warning(predictions(mod), pattern = "bootstrap")
options(marginaleffects_safe = op)


# set_coef.survreg(): `coefs` ends with one Log(scale) entry per stratum. An
# off-by-one used to write the scale into the last regression coefficient.
mod_scale <- survreg(Surv(time, status) ~ age + sex, data = survival::lung)
b <- get_coef(mod_scale)
mod_roundtrip <- set_coef(mod_scale, b)
expect_equivalent(coef(mod_roundtrip), coef(mod_scale))
expect_equivalent(mod_roundtrip$scale, mod_scale$scale)

mod_strata <- survreg(
    Surv(time, status) ~ age + strata(sex),
    data = survival::lung)
b <- get_coef(mod_strata)
mod_roundtrip <- set_coef(mod_strata, b)
expect_equivalent(coef(mod_roundtrip), coef(mod_strata))
expect_equivalent(mod_roundtrip$scale, mod_strata$scale)

# delta method vs. a Richardson-extrapolated Jacobian of exp(X %*% beta)
requiet("numDeriv")
dat_scale <- na.omit(survival::lung[, c("time", "status", "age", "sex")])
mod_scale <- survreg(Surv(time, status) ~ age + sex, data = dat_scale)
eps <- 1e-5
ame <- function(b, v) {
    hi <- lo <- dat_scale
    hi[[v]] <- dat_scale[[v]] + eps / 2
    lo[[v]] <- dat_scale[[v]] - eps / 2
    X_hi <- model.matrix(~ age + sex, hi)
    X_lo <- model.matrix(~ age + sex, lo)
    mean(exp(X_hi %*% b[1:3]) - exp(X_lo %*% b[1:3])) / eps
}
b <- c(coef(mod_scale), "Log(scale)" = log(mod_scale$scale))
V <- vcov(mod_scale)
slo <- avg_slopes(mod_scale)
for (v in c("age", "sex")) {
    J <- numDeriv::jacobian(function(x) ame(x, v), b, method = "Richardson")
    ref <- sqrt(as.numeric(J %*% V %*% t(J)))
    expect_equivalent(slo$std.error[slo$term == v], ref, tolerance = 1e-3)
}
