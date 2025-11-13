testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

set.seed(1024)
R <- 25
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

x <- mod |>
    avg_comparisons() |>
    inferences(method = "simulation", R = R)
expect_equal(nrow(x), 2, ignore_attr = TRUE)
x <- x |> get_draws()
expect_equal(nrow(x), 2 * R, ignore_attr = TRUE)

# Issue #851: simulation-based inference use the original estimates, not the mean/median of simulations
mod <- glm(vs ~ hp + mpg + am, data = mtcars, family = binomial)
cmp1 <- avg_comparisons(mod)
cmp2 <- cmp1 |> inferences(method = "simulation", R = 500)
expect_equal(cmp1$estimate, cmp2$estimate, ignore_attr = TRUE)

# mfxplainer bug
mod <- lm(mpg ~ hp + cyl, data = mtcars)
p <- avg_predictions(mod, by = "cyl") |> inferences(method = "simulation", R = 25)
expect_s3_class(p, "predictions")

# inferences with hypotheses
mod <- lm(mpg ~ hp + cyl, data = mtcars)
p <- hypotheses(mod, hypothesis = "hp/cyl=1") |> inferences(method = "simulation", R = 25)
expect_s3_class(p, "hypotheses")

# Clarify comparison
testthat::skip_if_not_installed("clarify")
testthat::skip_if_not_installed("MatchIt")
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
expect_equal(s1[1:2, 2], s3$conf.low, tolerance = .03, ignore_attr = TRUE)
expect_equal(s1[1:2, 3], s3$conf.high, tolerance = .03, ignore_attr = TRUE)

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
expect_equal(p2$estimate, p$estimate, ignore_attr = TRUE)

# Issue #1054
testthat::skip_if_not_installed("lme4")
requiet("lme4")
mod <- glmer(
    cbind(incidence, size - incidence) ~ period + (1 | herd),
    data = cbpp,
    family = binomial
)
cmp <- avg_comparisons(mod, variables = "period") |>
    inferences(method = "simulation", R = 15)
expect_s3_class(cmp, "comparisons")

# simulation-based inference
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)
x <- mod |>
    avg_predictions() |>
    inferences(method = "simulation", R = R)
expect_s3_class(x, "predictions")

x <- mod |>
    slopes() |>
    inferences(method = "simulation", R = R) |>
    head()
expect_s3_class(x, "slopes")

x <- mod |>
    predictions(vcov = "HC3") |>
    inferences(method = "simulation", R = R) |>
    head()
expect_s3_class(x, "predictions")

x <- mod |>
    comparisons() |>
    inferences(method = "simulation", R = R) |>
    components("draws")
expect_true(is.matrix(x))

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
