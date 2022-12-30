source("helpers.R", local = TRUE)

requiet("bife")

dat <<- bife::psid
mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = dat)

# marginaleffects: bife: no validity
mfx <- marginaleffects(mod)
tid <- tidy(mfx)
expect_inherits(tid, "data.frame")
expect_true("std.error" %in% colnames(tid))
expect_false(any(tid$estimate == 0))
expect_false(any(tid$std.error == 0))


# predictions: bife: no validity
pred <- predictions(mod)
expect_predictions(pred, n_row = nrow(dat))

