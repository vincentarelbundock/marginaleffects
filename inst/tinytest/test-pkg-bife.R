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



rm(list = ls())