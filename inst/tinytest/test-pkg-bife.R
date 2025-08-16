source("helpers.R")
exit_file("deprecated pending more tests")
using("marginaleffects")

requiet("bife")

mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = psid)
dat <- get_modeldata(mod, additional_variables = "KID")
dat <- psid <- transform(dat, KID = ifelse(KID1 > 0, 1, 0))

# marginaleffects: bife: no validity
expect_slopes(mod)

# predictions: bife: no validity
# bife does not store the call, so get_call() does not work and get_data() can
# only retrieve from model.frame, which is shorter than the full data.
pred <- predictions(mod, newdata = dat, type = "response")
expect_equivalent(pred$estimate, predict(mod, newdata = dat, type = "response"))
expect_predictions(mod, newdata = dat, type = "response", n_row = nrow(dat))


# Issue 809
mod <- bife(LFP ~ KID + AGE + KID * AGE + log(INCH) | ID, data = psid, model = "probit")

s1 <- slopes(mod, variables = "AGE", newdata = psid)
setorder(s1, rowid)
xb <- predict(mod, type = "link", X_new = psid)
s2 <- dnorm(xb) * (coef(mod)["AGE"] + coef(mod)["KID:AGE"] * psid$KID)
expect_equivalent(s1$estimate, s2, tolerance = 1e-4)

s3 <- slopes(mod, variables = "AGE", by = "KID", newdata = psid)
s2 <- aggregate(estimate ~ KID, FUN = mean, data = s1)
expect_equivalent(s3$estimate, s2$estimate, tolerance = 1e-4)

mod <- bife::bias_corr(mod)
s1 <- avg_slopes(mod, variables = "AGE", by = "KID", newdata = psid)
xb <- predict(mod, type = "link", X_new = psid)
psid$s <- dnorm(xb) * (coef(mod)["AGE"] + coef(mod)["KID:AGE"] * psid$KID)
s2 <- aggregate(s ~ KID, FUN = mean, data = psid)
s2 <- s2[order(s2$KID), ]
s1 <- s1[order(s1$KID), ]
expect_equivalent(s1$estimate, s2$s, tolerance = 1e-4)
