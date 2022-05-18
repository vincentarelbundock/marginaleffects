source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("bife")

# marginaleffects: bife: no validity
dataset <- bife::psid
mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = dataset)
mfx <- marginaleffects(mod)
tid <- tidy(mfx)
expect_inherits(tid, "data.frame")
expect_true("std.error" %in% colnames(tid))
expect_false(any(tid$estimate == 0))
expect_false(any(tid$std.error == 0))


# predictions: bife: no validity
dataset <- bife::psid
mod <- bife(LFP ~ AGE + I(INCH / 1000) + KID1 + KID2 + KID3 | ID, data = dataset)
pred <- predictions(mod)
expect_predictions(pred, n_row = nrow(insight::get_data(mod)))

