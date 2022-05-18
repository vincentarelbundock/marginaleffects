source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("robustbase")
requiet("margins")

# lmrob vs. margins
data(coleman, package = "robustbase")
model <- lmrob(Y ~ ., data=coleman, setting = "KS2014")
expect_marginaleffects(model, n_unique = 1)
mar <- margins::margins(model, unit_ses = TRUE)
mfx <- marginaleffects(model)
expect_true(expect_margins(mar, mfx))

# glmrob vs. margins
data(epilepsy, package = "robustbase")
model <- glmrob(Ysum ~ Age10 + Base4*Trt, family = poisson,
                data = epilepsy, method= "Mqle",
                control = glmrobMqle.control(tcc= 1.2))
expect_marginaleffects(model)
mar <- margins::margins(model, unit_ses = TRUE)
mfx <- marginaleffects(model)
expect_true(expect_margins(mar, mfx))
