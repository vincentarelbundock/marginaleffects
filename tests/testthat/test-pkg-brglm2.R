testthat::skip_if_not_installed("brglm2")
testthat::skip_if_not_installed("margins")
testthat::skip_if_not_installed("emmeans")
testthat::skip_if_not_installed("broom")

requiet("brglm2")
requiet("margins")
requiet("emmeans")
requiet("broom")

# Basic expectation tests
mod_simple <- glm(am ~ mpg + wt, data = mtcars, family = binomial(), method = "brglm_fit")
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# brglm2::brglm_fit vs. margins vs. emtrends
data("endometrial", package = "brglm2", envir = environment())
dat <<- endometrial
model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
model <- update(model, method = "brglm_fit") # probably breaks get_data from environemnt


# margins
mar <- margins(model)
mfx <- slopes(model, newdata = dat)
expect_slopes2(model, newdata = dat)
expect_margins2(mar, mfx)
# emtrends
em <- emtrends(model, ~PI, "PI", at = list(PI = 15, EH = 2, NV = 0))
em <- tidy(em)
mfx <- slopes(
    model,
    variables = "PI",
    newdata = datagrid(PI = 15, EH = 2, NV = 0),
    type = "link"
)
expect_equal(mfx$estimate, em$PI.trend, ignore_attr = TRUE)
expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)


# predictions: brglm2::brglm_fit: no validity
data("endometrial", package = "brglm2", envir = environment())
dat <- endometrial
model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
model <- update(model, method = "brglm_fit")
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(endometrial))
expect_predictions2(model, n_row = nrow(endometrial))
expect_predictions2(model, newdata = head(endometrial), n_row = 6)


# brmultinom: no validity
testthat::skip_if_not_installed("MASS")
data("housing", package = "MASS")
dat <<- housing
mod <- brmultinom(Sat ~ Infl + Type + Cont, weights = Freq, data = housing, type = "ML", ref = 1)
expect_slopes2(mod, type = "probs")
expect_predictions2(mod, type = "probs")


# bracl: no validity
data("stemcell", package = "brglm2")
dat <- stemcell
dat$religion <- as.numeric(dat$religion)
mod <- bracl(
    research ~ as.numeric(religion) + gender,
    weights = frequency,
    data = dat,
    type = "ML"
)
expect_predictions2(mod, type = "probs")
expect_slopes2(mod, type = "probs")


# brglm2::brglm_fit vs. margins
tmp <<- data.frame(
    freq = c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 21, 33, 60, 41, 42),
    dose = rep(c(0, 10, 33, 100, 333, 1000), 3),
    observation = rep(1:3, each = 6)
)
model <- brnb(
    freq ~ dose + log(dose + 10),
    data = tmp,
    link = "log",
    transformation = "inverse",
    type = "ML"
)
expect_slopes2(model, n_unique = 6, newdata = tmp)
mfx <- suppressWarnings(slopes(model))
mar <- suppressWarnings(margins(model))
expect_margins2(mar, mfx)
