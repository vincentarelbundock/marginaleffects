source("helpers.R")
requiet("haven")
requiet("Rchoice")

# hetprob() dy/dx
dat <<- transform(iris, y = Sepal.Length > median(Sepal.Length))
mod <- hetprob(y ~ Petal.Width * Petal.Length | factor(Species), data = dat, link = "logit")
known <- Rchoice::effect(mod)$margins
mfx <- avg_slopes(mod)
expect_equivalent(sort(mfx$estimate), sort(known[, "dydx"]), tol = .001)
expect_equivalent(sort(mfx$std.error), sort(known[, "Std. error"]), tol = .001)

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


source("helpers.R")
rm(list = ls())