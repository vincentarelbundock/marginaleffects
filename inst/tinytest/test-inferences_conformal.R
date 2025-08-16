source("helpers.R")
if (!EXPENSIVE) exit_file("EXPENSIVE")
using("marginaleffects")

# Issue #1407: conformal inference with `residual_sq` scores.
set.seed(48103)
dat = get_dataset("military")
idx = sample(c("train", "calibration", "test"), nrow(dat), replace = TRUE)
dat = split(dat, idx)
train = dat$train
calib = dat$calibration
test = dat$test
mod = lm(rank ~ grade + branch + gender + race, data = train)
p = predictions(mod, conf_level = 0.9) |>
    inferences(
        method = "conformal_split",
        conformal_calibration = calib,
        conformal_score = "residual_abs",
        conformal_test = test
    )
coverage = mean(p$rank > p$pred.low & p$rank < p$pred.high)
expect_equivalent(round(coverage, 2), .9)
p = predictions(mod, conf_level = 0.9) |>
    inferences(
        method = "conformal_split",
        conformal_calibration = calib,
        conformal_score = "residual_sq",
        conformal_test = test
    )
coverage = mean(p$rank > p$pred.low & p$rank < p$pred.high)
expect_equivalent(round(coverage, 2), .9)