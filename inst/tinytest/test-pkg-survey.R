source("helpers.R")
using("marginaleffects")

requiet("margins")
requiet("emmeans")
requiet("broom")
requiet("survey")

# Basic expectation tests
mod_simple <- lm(mpg ~ wt + am, data = mtcars)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

# survey: marginaleffects vs. margins vs. emtrends
data("fpc", package = "survey")
svyd <- survey::svydesign(
    weights = ~weight,
    ids = ~psuid,
    strata = ~stratid,
    fpc = ~Nh,
    variables = ~ x + nh,
    data = fpc,
    nest = TRUE
)
mod <- survey::svyglm(x ~ nh, design = svyd)
res <- slopes(mod, wts = "(weights)")
mar <- suppressMessages(data.frame(margins(mod, unit_ses = TRUE)))
expect_equivalent(res$estimate, as.numeric(mar$dydx_nh))
expect_equivalent(res$std.error, as.numeric(mar$SE_dydx_nh), tolerance = 0.001)
# emtrends
em <- emtrends(mod, ~nh, "nh", at = list(nh = 4))
em <- tidy(em)
mfx <- slopes(mod, type = "link", newdata = data.frame(nh = 4))
expect_equivalent(mfx$estimate, em$nh.trend, tolerance = .001) # CRAN tolerance
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)


# Issue #1131
data("lalonde", package = "MatchIt")
fit <- survey::svyglm(re78 ~ treat, design = survey::svydesign(~1, weights = ~1, data = lalonde))
p <- marginaleffects::get_predict(fit, newdata = lalonde)
expect_inherits(p, "data.frame")


# Issue #1161
dat <- "https://vincentarelbundock.github.io/Rdatasets/csv/AER/SmokeBan.csv"
dat <- read.csv(dat, na.strings = c("*", ""))
dat$weights <- runif(n = nrow(dat), min = 1, max = 100)
dat$smoker <- factor(dat$smoker)
design1 = svydesign(ids = ~1, weights = ~weights, data = dat)
m <- suppressWarnings(svyglm(
    smoker ~ ban * education * gender + age,
    design = design1,
    family = binomial(),
    data = dat
))
cmp <- avg_comparisons(m, variables = "education", by = c("ban", "gender"), wts = "weights", hypothesis = ~reference)
expect_false(anyNA(cmp$estimate))
