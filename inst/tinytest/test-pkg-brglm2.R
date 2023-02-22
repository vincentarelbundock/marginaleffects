source("helpers.R")
using("marginaleffects")

requiet("brglm2")
requiet("margins")
requiet("emmeans")
requiet("broom")

# brglm2::brglm_fit vs. margins vs. emtrends
data("endometrial", package = "brglm2", envir = environment())
dat <- endometrial
model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
model <- update(model, method = "brglm_fit") # probably breaks get_data from environemnt


# margins
mar <- margins(model)
mfx <- slopes(model, newdata = dat)
expect_slopes(model, newdata = dat)
expect_margins(mar, mfx)
# emtrends
em <- emtrends(model, ~PI, "PI", at = list(PI = 15, EH = 2, NV = 0))
em <- tidy(em)
mfx <- slopes(
    model,
    variables = "PI",
    newdata = datagrid(PI = 15, EH = 2, NV = 0), 
    type = "link")
expect_equivalent(mfx$estimate, em$PI.trend)
expect_equivalent(mfx$std.error, em$std.error, tolerance = .00001)


# brglm2::brglm_fit vs. margins
sm <- data.frame(
    freq = c(15, 16, 16, 27, 33, 20, 21, 18, 26, 41, 38, 27, 29, 21, 33, 60, 41, 42),
    dose = rep(c(0, 10, 33, 100, 333, 1000), 3),
    observation = rep(1:3, each = 6))
model <- brnb(
    freq ~ dose + log(dose + 10),
    data = sm,
    link = "log",
    transformation = "inverse",
    type = "ML")
expect_slopes(model, n_unique = 6, newdata = sm)
mfx <- suppressWarnings(slopes(model))
mar <- suppressWarnings(margins(model))
expect_margins(mar, mfx)


# predictions: brglm2::brglm_fit: no validity
data("endometrial", package = "brglm2", envir = environment())
dat <- endometrial
model <- glm(HG ~ NV + PI + EH, family = binomial("probit"), data = dat)
model <- update(model, method = "brglm_fit")
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(endometrial))
expect_predictions(pred1, n_row = nrow(endometrial))
expect_predictions(pred2, n_row = 6)



# brmultinom: no validity
data("housing", package = "MASS")
mod <- brmultinom(Sat ~ Infl + Type + Cont, weights = Freq,
              data = housing, type = "ML", ref = 1)
expect_slopes(mod, type = "probs")
expect_predictions(predictions(mod, type = "probs"))



# bracl: no validity
data("stemcell", package = "brglm2")
dat <- stemcell
dat$religion <- as.numeric(dat$religion)
mod <- bracl(
    research ~ as.numeric(religion) + gender, weights = frequency,
    data = dat, type = "ML")
expect_predictions(predictions(mod, type = "probs"))
expect_slopes(mod, type = "probs")




rm(list = ls())