requiet("haven")
requiet("Rchoice")

# Basic expectation tests
dat <- transform(iris, y = Sepal.Length > median(Sepal.Length))
mod_simple <- hetprob(y ~ Petal.Width * Petal.Length | factor(Species),
    data = dat, link = "logit")
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

# hetprob() dy/dx
dat <- transform(iris, y = Sepal.Length > median(Sepal.Length))
mod <- hetprob(y ~ Petal.Width * Petal.Length | factor(Species), data = dat, link = "logit")
known <- Rchoice::effect(mod)$margins
mfx <- avg_slopes(mod)
expect_equal(sort(mfx$estimate), sort(known[, "dydx"]), tolerance = .001, ignore_attr = TRUE)
expect_equal(sort(mfx$std.error), sort(known[, "Std. error"]), tolerance = .001, ignore_attr = TRUE)

# # IV probit model by MLE
# # (nwincome is endogenous and heducation is the additional instrument)
# dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/PSID1976.csv")
# dat$nwincome <- with(dat, (fincome - hours * wage)/1000)
# dat$lfp <- as.numeric(dat$participation == "yes")
# mod <- ivpml(
#     lfp ~ education + experience + I(experience^2) + age + youngkids + oldkids + nwincome |
#           education + experience + I(experience^2) + age + youngkids + oldkids + heducation,
#     data = dat,
#     message = FALSE)
#
# effect(mod, asf = FALSE)$margins %>% data.frame() %>% arrange(rownames(.))
# effect(mod, asf = TRUE)$margins %>% data.frame() %>% arrange(rownames(.))
# avg_slopes(mod)

# h <- 1e-4
# dat_lo <- transform(dat, nwincome = nwincome - h / 2)
# dat_hi <- transform(dat, nwincome = nwincome + h / 2)
# p_lo <- predict(mod, newdata = dat_lo)
# p_hi <- predict(mod, newdata = dat_hi)
# mean((p_hi - p_lo) / h)
