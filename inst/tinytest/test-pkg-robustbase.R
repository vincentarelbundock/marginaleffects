source("helpers.R")
using("marginaleffects")

requiet("robustbase")
requiet("margins")

# lmrob vs. margins
data(coleman, package = "robustbase")
model <- lmrob(Y ~ ., data=coleman, setting = "KS2014")
expect_slopes(model, n_unique = 1)
mar <- margins::margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_true(expect_margins(mar, mfx))

# glmrob vs. margins
data(epilepsy, package = "robustbase")
model <- glmrob(Ysum ~ Age10 + Base4*Trt, family = poisson,
                data = epilepsy, method= "Mqle",
                control = glmrobMqle.control(tcc= 1.2))
expect_slopes(model)
mar <- margins::margins(model, unit_ses = TRUE)
mfx <- slopes(model)
expect_true(expect_margins(mar, mfx))



rm(list = ls())