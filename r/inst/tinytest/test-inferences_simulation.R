source("helpers.R")
using("marginaleffects")

set.seed(1024)
R <- 25
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

draws <- matrix(rnorm(200), nrow = 10)
draw_summary <- marginaleffects:::inferences_simulation_summarize(
    draws,
    conf_level = 0.9,
    interval = TRUE
)
expect_equivalent(draw_summary$std.error, apply(draws, 1, stats::sd))
expect_equivalent(
    draw_summary$conf.low,
    apply(draws, 1, stats::quantile, probs = 0.05, names = FALSE)
)
expect_equivalent(
    draw_summary$conf.high,
    apply(draws, 1, stats::quantile, probs = 0.95, names = FALSE)
)
draws_missing <- draws
draws_missing[1, 1] <- NA_real_
expect_error(
    marginaleffects:::inferences_simulation_summarize(draws_missing, 0.9, TRUE),
    pattern = "missing values"
)

simulation_full_draws <- function(x, R, seed) {
    mfx <- attr(x, "marginaleffects")
    model <- mfx@model
    V <- mfx@vcov_model
    if (!is.matrix(V)) {
        V <- marginaleffects:::get_vcov(model)
    }

    set.seed(seed)
    coefmat <- mvtnorm::rmvnorm(R, marginaleffects:::get_coef(model), V)
    call_mfx <- mfx@call
    call_mfx[["vcov"]] <- FALSE
    call_mfx[["modeldata"]] <- mfx@modeldata
    call_mfx[["newdata"]] <- mfx@newdata

    vapply(seq_len(R), function(i) {
        call_mfx[["model"]] <- marginaleffects:::set_coef(model, coefmat[i, ])
        eval(call_mfx, envir = parent.frame())$estimate
    }, numeric(nrow(x)))
}

mod_replay <- lm(mpg ~ hp + cyl, data = mtcars)
pred_replay <- avg_predictions(mod_replay, by = "cyl", transform = exp)
seed_replay <- 918
expected_replay <- simulation_full_draws(pred_replay, R = 8, seed = seed_replay)
set.seed(seed_replay)
actual_replay <- inferences(pred_replay, method = "simulation", R = 8)
expect_equivalent(
    attr(actual_replay, "marginaleffects")@draws,
    expected_replay,
    tolerance = 1e-10
)

cmp_replay <- avg_comparisons(mod_replay, by = "cyl", transform = exp)
expected_replay <- simulation_full_draws(cmp_replay, R = 8, seed = seed_replay)
set.seed(seed_replay)
actual_replay <- inferences(cmp_replay, method = "simulation", R = 8)
expect_equivalent(
    attr(actual_replay, "marginaleffects")@draws,
    expected_replay,
    tolerance = 1e-10
)
expect_null(marginaleffects:::settings_get(
    "marginaleffects_capture_simulation_replay"
))

set.seed(seed_replay)
direct_replay <- avg_predictions(
    mod_replay,
    by = "cyl",
    vcov = "simulation"
)
expect_equal(
    dim(attr(direct_replay, "marginaleffects")@draws),
    c(3L, 1000L)
)
expect_null(attr(
    attr(direct_replay, "marginaleffects"),
    "marginaleffects_simulation_replay"
))

x <- mod |>
    avg_comparisons() |>
    inferences(method = "simulation", R = R)
expect_equivalent(nrow(x), 2)
x <- x |> get_draws()
expect_equivalent(nrow(x), 2 * R)

# Issue #851: simulation-based inference use the original estimates, not the mean/median of simulations
mod <- glm(vs ~ hp + mpg + am, data = mtcars, family = binomial)
cmp1 <- avg_comparisons(mod)
cmp2 <- cmp1 |> inferences(method = "simulation", R = 500)
expect_equivalent(cmp1$estimate, cmp2$estimate)

# mfxplainer bug
mod <- lm(mpg ~ hp + cyl, data = mtcars)
p <- avg_predictions(mod, by = "cyl") |> inferences(method = "simulation", R = 25)
expect_inherits(p, "predictions")

# inferences with hypotheses
mod <- lm(mpg ~ hp + cyl, data = mtcars)
p <- hypotheses(mod, hypothesis = "hp/cyl=1") |> inferences(method = "simulation", R = 25)
expect_inherits(p, "hypotheses")

# Clarify comparison
requiet("clarify")
requiet("MatchIt")
data("lalonde", package = "MatchIt")
set.seed(1025)
fit <- glm(
    I(re78 == 0) ~ treat * (age + educ + race + married + nodegree + re74 + re75),
    data = lalonde,
    family = binomial
)
sim_coefs <- clarify::sim(fit)
ATT_fun <- function(fit) {
    d <- subset(lalonde, treat == 1)
    d$treat <- 1
    p1 <- mean(predict(fit, newdata = d, type = "response"))
    d$treat <- 0
    p0 <- mean(predict(fit, newdata = d, type = "response"))
    c(`E[Y(0)]` = p0, `E[Y(1)]` = p1, `RR` = p1 / p0)
}
sim_est <- sim_apply(sim_coefs, ATT_fun, verbose = FALSE)
s1 <- summary(sim_est)
s3 <- avg_predictions(fit, variables = "treat", type = "response", newdata = subset(lalonde, treat == 1)) |>
    inferences(method = "simulation", R = 1000)
expect_equivalent(s1[1:2, 2], s3$conf.low, tolerance = .03)
expect_equivalent(s1[1:2, 3], s3$conf.high, tolerance = .03)

# issue #1124: inferences is on the correct scale
set.seed(1024)
dat <- read.csv(testing_path("modelarchive/data/impartiality.csv"))
m <- glm(
    impartial ~ equal * democracy + continent,
    data = dat,
    family = binomial

)
p <- predictions(m, by = "democracy", type = "response") |>
    inferences(method = "simulation", R = 100)
expect_true(all(p$estimate > 0 & p$estimate < 1))
expect_true(all(p$conf.low > 0 & p$conf.low < 1))
expect_true(all(p$conf.high > 0 & p$conf.high < 1))
expect_true(all(p$conf.low < p$estimate & p$conf.high > p$estimate))

p2 <- predictions(m, by = "democracy", type = "response")
expect_equivalent(p2$estimate, p$estimate)

# Issue #1054
requiet("lme4")
mod <- glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial
)
cmp <- avg_comparisons(mod, variables = "period") |>
    inferences(method = "simulation", R = 15)
expect_inherits(cmp, "comparisons")

# simulation-based inference
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)
x <- mod |>
    avg_predictions() |>
    inferences(method = "simulation", R = R)
expect_inherits(x, "predictions")

x <- mod |>
    slopes() |>
    inferences(method = "simulation", R = R) |>
    head()
expect_inherits(x, "slopes")

x <- mod |>
    predictions(vcov = "HC3") |>
    inferences(method = "simulation", R = R) |>
    head()
expect_inherits(x, "predictions")

x <- mod |>
    comparisons() |>
    inferences(method = "simulation", R = R) |>
    components("draws")
expect_inherits(x, "matrix")

# simulation-based inference respects `vcov` argument
mod <- lm(mpg ~ hp + cyl, data = mtcars)

set.seed(48103)
p1 <- avg_predictions(mod, vcov = "HC3") |>
    inferences(method = "simulation", R = R)
set.seed(48103)
p2 <- avg_predictions(mod, vcov = "HC3") |>
    inferences(method = "simulation", R = R)
set.seed(48103)
p3 <- avg_predictions(mod) |>
    inferences(method = "simulation", R = R)
expect_true(all(p1$conf.low == p2$conf.low))
expect_true(all(p1$conf.low != p3$conf.low))

set.seed(48103)
p1 <- avg_comparisons(mod, vcov = "HC3") |>
    inferences(method = "simulation", R = R)
set.seed(48103)
p2 <- avg_comparisons(mod, vcov = "HC3") |>
    inferences(method = "simulation", R = R)
set.seed(48103)
p3 <- avg_comparisons(mod) |>
    inferences(method = "simulation", R = R)
expect_true(all(p1$conf.low == p2$conf.low))
expect_true(all(p2$conf.low != p3$conf.low))

set.seed(48103)
h1 <- hypotheses(mod, hypothesis = "hp/cyl=1", vcov = "HC3") |>
    inferences(method = "simulation", R = 25)
set.seed(48103)
h2 <- hypotheses(mod, hypothesis = "hp/cyl=1", vcov = "HC3") |>
    inferences(method = "simulation", R = 25)
set.seed(48103)
h3 <- hypotheses(mod, hypothesis = "hp/cyl=1") |>
    inferences(method = "simulation", R = 25)
expect_true(all(h1$conf.low == h2$conf.low))
expect_true(all(h2$conf.low != h3$conf.low))
