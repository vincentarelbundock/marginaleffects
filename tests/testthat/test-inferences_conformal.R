testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

testthat::skip_if_not_installed("probably")
testthat::skip_if_not_installed("workflows")
testthat::skip_if_not_installed("parsnip")
testthat::skip_if_not_installed("tidymodels")
testthat::skip_if_not_installed("dplyr")

requiet("probably")
requiet("workflows")
requiet("parsnip")
requiet("tidymodels")
requiet("dplyr")

# Issue #1407: conformal inference with `residual_sq` scores.
set.seed(48103)
dat = get_dataset("military")
dat_small = dat[sample(1:nrow(dat), 2000), ]
idx = sample(c("train", "calibration", "test"), nrow(dat_small), replace = TRUE)
dat = split(dat_small, idx)

mod = lm(rank ~ grade + branch + gender + race, data = dat$train)
p = predictions(mod, newdata = dat$test, conf_level = 0.9) |>
    inferences(
        method = "conformal_split",
        data_calib = dat$calib,
        conformal_score = "residual_abs"
    )
coverage = mean(p$rank > p$pred.low & p$rank < p$pred.high)
expect_equal(coverage, .9, tolerance = 1e-2, ignore_attr = TRUE)


# full conformal inference: validate approximate coverage
set.seed(48103)
idx <- sample(c("train", "test"), nrow(iris), replace = TRUE)
dat <- split(iris, idx)
mod <- lm(Sepal.Length ~ ., data = dat$train)
p <- predictions(mod, newdata = dat$test, conf_level = .9) |>
    inferences(method = "conformal_full")
coverage <- mean(p$Sepal.Length > p$pred.low & p$Sepal.Length < p$pred.high)
expect_equal(coverage, .9, tolerance = 2e-2, ignore_attr = TRUE)


###### conformal quantile: validate against {probably} package
set.seed(48103)
dat = get_dataset("military")
dat_small = dat[sample(1:nrow(dat), 1000), ]
idx = sample(c("train", "calibration", "test"), nrow(dat_small), replace = TRUE)
dat = split(dat_small, idx)

# Convert rank to numeric for both implementations
dat$train$rank <- as.numeric(dat$train$rank)
dat$calibration$rank <- as.numeric(dat$calibration$rank)
dat$test$rank <- as.numeric(dat$test$rank)

# marginaleffects implementation
mod = lm(rank ~ grade + branch + gender + race, data = dat$train)
p_mfx = predictions(mod, newdata = dat$test, conf_level = 0.8) |>
    inferences(
        method = "conformal_quantile",
        data_train = dat$train,
        data_calib = dat$calibration,
        nthreads = 1
    )
coverage_mfx = mean(p_mfx$rank > p_mfx$pred.low & p_mfx$rank < p_mfx$pred.high)

testthat::skip("probably issue #198")
# probably implementation
lm_spec <- parsnip::linear_reg() |>
    parsnip::set_mode("regression") |>
    parsnip::set_engine("lm")
lm_wflow <- workflows::workflow() |>
    workflows::add_model(lm_spec) |>
    workflows::add_formula(rank ~ grade + branch + gender + race)
lm_fit <- workflows::fit(lm_wflow, data = dat$train)
conf_int <- probably::int_conformal_quantile(
    lm_fit,
    train_data = dat$train,
    cal_data = dat$calibration,
    level = 0.8,
    nthreads = 1
)
preds_prob <- predict(conf_int, dat$test)
coverage_prob = mean(dat$test$rank > preds_prob$.pred_lower &
    dat$test$rank < preds_prob$.pred_upper)
expect_equal(coverage_mfx, coverage_prob, tolerance = 1e-5, ignore_attr = TRUE)
