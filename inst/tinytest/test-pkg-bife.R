source("helpers.R")
using("marginaleffects")

requiet("bife")

dat <- bife::psid
mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = dat)

dat <- data.frame(dat)

# marginaleffects: bife: no validity
expect_slopes(mod)

# predictions: bife: no validity
# bife does not store the call, so get_call() does not work and get_data() can
# only retrieve from model.frame, which is shorter than the full data.
pred <- predictions(mod, newdata = dat)
expect_predictions(pred, n_row = nrow(dat))


# Issue 809
data(psid, package = "bife")

psid <- transform(psid, KID = ifelse(KID1 > 0, 1, 0))
mod <- bife(LFP ~ KID + AGE + KID * AGE + log(INCH) | ID, data = psid, model = "probit")

s1 <- avg_slopes(mod, variables = "AGE", by = "KID", newdata = psid)
xb <- predict(mod, type = "link", X_new = psid)
psid$s <- dnorm(xb) * (coef(mod)["AGE"] + coef(mod)["KID:AGE"] * psid$KID)
s2 <- aggregate(s ~ KID, FUN = mean, data = psid)
s2 <- s2[order(s2$KID),]
s1 <- s1[order(s1$KID),]
expect_equivalent(s1$estimate, s2$s, tolerance = 1e-4)

mod <- bife::bias_corr(mod)
s1 <- avg_slopes(mod, variables = "AGE", by = "KID", newdata = psid)
xb <- predict(mod, type = "link", X_new = psid)
psid$s <- dnorm(xb) * (coef(mod)["AGE"] + coef(mod)["KID:AGE"] * psid$KID)
s2 <- aggregate(s ~ KID, FUN = mean, data = psid)
s2 <- s2[order(s2$KID),]
s1 <- s1[order(s1$KID),]
expect_equivalent(s1$estimate, s2$s, tolerance = 1e-4)



rm(list = ls())