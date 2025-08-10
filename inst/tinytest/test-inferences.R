source("helpers.R")
if (!EXPENSIVE) exit_file("expensive")

# inferences() currently returns a `comparisons` object even with `slopes()`

set.seed(1024)
R <- 25
mod <- lm(Petal.Length ~ Sepal.Length * Sepal.Width, data = iris)

# simulation-based inference
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
    attr("posterior_draws")
expect_inherits(x, "matrix")


set.seed(1234)
# {boot}
x <- mod |>
    avg_predictions() |>
    inferences(method = "boot", R = R)
expect_inherits(x, "predictions")
expect_equivalent(nrow(x), 1)


# head works
set.seed(1234)
x <- mod |>
    slopes() |>
    inferences(method = "boot", R = R)
expect_inherits(head(x), "slopes")
expect_equivalent(nrow(x), 300)
expect_equivalent(nrow(head(x)), 6)

# avg_ works
set.seed(1234)
x <- mod |>
    avg_slopes() |>
    inferences(method = "boot", R = R)
expect_inherits(x, "slopes") # should be slopes
expect_equivalent(nrow(x), 2)


x <- mod |>
    predictions(vcov = "HC3") |>
    inferences(method = "boot", R = R) |>
    head()
expect_inherits(x, "predictions")
x <- mod |>
    comparisons() |>
    inferences(method = "boot", R = R) |>
    attr("inferences")
expect_inherits(x, "boot")
nd <- datagrid(Sepal.Length = range, model = mod)
x <- mod |>
    comparisons(variables = "Sepal.Width", newdata = nd) |>
    inferences(method = "boot", R = R)
expect_equivalent(nrow(x), 2)
x <- mod |>
    avg_comparisons() |>
    inferences(method = "simulation", R = R)
expect_equivalent(nrow(x), 2)
x <- x |> get_draws()
expect_equivalent(nrow(x), 2 * R)


# {rsample}
set.seed(1234)
x <- mod |>
    avg_predictions() |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_equal(x$conf.low, 3.665, tolerance = 1e-3)
expect_inherits(x, "predictions")
x <- mod |>
    slopes() |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_inherits(x, "slopes")
x <- mod |>
    predictions(vcov = "HC3") |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_inherits(x, "predictions")
x <- mod |>
    comparisons() |>
    inferences(method = "rsample", R = R) |>
    attr("inferences") |>
    suppressWarnings()
expect_inherits(x, "bootstraps")
nd <- datagrid(Sepal.Length = range, model = mod)
x <- mod |>
    comparisons(variables = "Sepal.Width", newdata = nd) |>
    inferences(method = "rsample", R = R) |>
    suppressWarnings()
expect_equivalent(nrow(x), 2)
x <- mod |>
    avg_comparisons() |>
    inferences(method = "rsample", R = R) |>
    get_draws() |>
    suppressWarnings()
expect_equivalent(nrow(x), 2 * R)

# fwb no validity check
exit_file("Issue $#6 on fwb")
set.seed(1234)
x <- mod |>
    comparisons() |>
    inferences(method = "fwb", R = R) |> suppressWarnings()
expect_equivalent(nrow(x), 300)
expect_equal(x$std.error[1:3], c(0.0642131648304821, 0.0444891291752277, 0.0442572266844693))
x <- mod |>
    avg_comparisons() |>
    inferences(method = "fwb", R = R) |> suppressWarnings()
expect_equivalent(nrow(x), 2)


# {fwb} error when user supplied its own weightso
dat <- transform(mtcars, w = runif(32))
mod <- lm(mpg ~ hp, data = dat)
expect_error(inferences(comparisons(mod, wts = "w"), method = "fwb"), pattern = "wts")


# Issue #856
tmp <- lm(Petal.Length ~ Sepal.Length * Species, data = iris)
cmp <- avg_comparisons(tmp, variables = list(Sepal.Length = 1, Species = "reference"), cross = TRUE) |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_inherits(cmp, "comparisons")
expect_equal(nrow(cmp), 2)


# Issue #853
m <- glm(am ~ mpg + hp + cyl, data = mtcars, family = binomial)
p <- avg_predictions(m, by = "cyl") |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_inherits(p, "predictions")
p <- predictions(m, by = "cyl") |>
    inferences(method = "boot", R = 5) |>
    suppressWarnings()
expect_inherits(p, "predictions")


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
p <- hypotheses(mod, hypothesis = "hp/cyl=1") |>
    inferences(method = "boot", R = 25) |>
    suppressWarnings()
expect_inherits(p, "hypotheses")
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


# simulation-based inference respects `vcov` argument
mod <- lm(mpg ~ hp + cyl, data = mtcars)
set.seed(48103)
h1 <- hypotheses(mod, hypothesis = "hp/cyl=1", vcov = "HC3") |>
    inferences(method = "simulation", R = 25)
set.seed(48103)
h2 <- hypotheses(mod, hypothesis = "hp/cyl=1", vcov = "HC3") |>
    inferences(method = "simulation", R = 25)
set.seed(48103)
h3 <- hypotheses(mod, hypothesis = "hp/cyl=1") |>
    inferences(method = "simulation", R = 25)
expect_equivalent(h1$conf.low, h2$conf.low)
expect_equivalent(h1$conf.high, h2$conf.high)
expect_false(ignore(expect_equivalent)(h1$conf.low, h3$conf.low))
expect_false(ignore(expect_equivalent)(h1$conf.high, h3$conf.high))

# Issue #1407: conformal inference with `residual_sq` scores.
set.seed(48103)
dat = get_dataset("military")
idx = sample(c("train", "calibration", "test"), nrow(dat), replace = TRUE)
dat = split(dat, idx)
train = dat$train
calib = dat$calibration
test = dat$test
mod = lm(rank ~ grade + branch + gender + race, data = train)
p = predictions(mod, conf_level = 0.9) |>
    inferences(
        method = "conformal_split",
        conformal_calibration = calib,
        conformal_score = "residual_abs",
        conformal_test = test
    )
coverage = mean(p$rank > p$pred.low & p$rank < p$pred.high)
expect_equivalent(round(coverage, 2), .9)
p = predictions(mod, conf_level = 0.9) |>
    inferences(
        method = "conformal_split",
        conformal_calibration = calib,
        conformal_score = "residual_sq",
        conformal_test = test
    )
coverage = mean(p$rank > p$pred.low & p$rank < p$pred.high)
expect_equivalent(round(coverage, 2), .9)

# Bug: rsample collapses non-unique term
mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
k <- avg_comparisons(mod) |> inferences(method = "rsample", R = R) |> suppressWarnings()
expect_inherits(k, "comparisons")


# `estimator` function
lalonde <- get_dataset("lalonde")
estimator <- function(data) {
    fit1 <- glm(treat ~ age + educ + race, family = binomial, data = data)
    ps <- predict(fit1, type = "response") 
    m <- lm(re78 ~ treat * (re75 + age + educ + race), data = data, weight = ps)
    avg_comparisons(m, variables = "treat", wts = ps, vcov = FALSE)
}
cmp <- inferences(lalonde, method = "rsample", estimator = estimator, R = R) |>
    suppressWarnings()
expect_inherits(cmp, "comparisons")
expect_error(inferences(lalonde, method = "rsample"), "when supplying a function to the `estimator` argument.")
expect_error(inferences(estimator(lalonde), estimator = estimator, method = "rsample"), "The `x` argument must be a raw data frame when using the `estimator` argument.")
expect_false(ignore(expect_error)(inferences(lalonde, method = "rsample", estimator = estimator, R = 3))) |> suppressWarnings()


# survival vignette
requiet("survival")
requiet("splines")
model <- coxph(
    Surv(dtime, death) ~ hormon * factor(grade) + ns(age, df = 2),
    data = rotterdam
)
nd <- datagrid(
    hormon = unique,
    grade = unique,
    dtime = seq(36, 7043, length.out = 25),
    grid_type = "counterfactual",
    model = model
)

p <- predictions(model, type = "survival", by = c("dtime", "hormon", "grade"), newdata = nd)
p <- inferences(p, method = "rsample", R = R) |> suppressWarnings()
expect_true(all(p$estimate >= p$conf.low))
expect_true(all(p$estimate <= p$conf.high))


# # works interactively
# p <- predictions(model, type = "survival", by = c("dtime", "hormon", "grade"), vcov = "rsample", newdata = nd)
# expect_true(all(p$estimate >= p$conf.low))
# expect_true(all(p$estimate <= p$conf.high))
