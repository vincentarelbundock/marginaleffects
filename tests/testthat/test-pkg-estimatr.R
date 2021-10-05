skip_if_not_installed("estimatr")
requiet("estimatr")

test_that("iv_robust vs. stata", {
    data(Kmenta, package = "ivreg")
    model <- iv_robust(Q ~ P + D | D + F + A, 
                       se_type = "stata",
                       data = Kmenta)
    stata <- readRDS(test_path("stata/stata.rds"))$estimatr_iv_robust
    mfx <- tidy(marginaleffects(model))
    mfx <- merge(mfx, stata)
    expect_equal(mfx$dydx, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1)
})


test_that("lm_robust vs. stata", {
    model <- lm_robust(carb ~ wt + factor(cyl), 
                       se_type = "stata",
                       data = mtcars)
    stata <- readRDS(test_path("stata/stata.rds"))$estimatr_lm_robust
    mfx <- tidy(marginaleffects(model))
    mfx <- merge(mfx, stata)
    expect_equal(mfx$dydx, mfx$dydxstata)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .01)
})


test_that("iv_robust: predictions: no validity", {
    data(Kmenta, package = "ivreg")
    model <- iv_robust(Q ~ P + D | D + F + A, 
                       se_type = "stata",
                       data = Kmenta)
    expect_predictions(predictions(model), se = FALSE, n_row = 1)
    expect_predictions(predictions(model, newdata = head(Kmenta)), se = FALSE, n_row = 6)
})

test_that("lm_robust: marginalmeans predictions: no validity", {
    tmp <- mtcars
    tmp$cyl <- as.factor(tmp$cyl)
    tmp$am <- as.logical(tmp$am)
    model <- lm_robust(carb ~ wt + am + cyl,
                       se_type = "stata",
                       data = tmp)
    expect_predictions(predictions(model), se = FALSE, n_row = 1)
    expect_predictions(predictions(model, newdata = head(tmp)), se = FALSE, n_row = 6)
    expect_marginalmeans(marginalmeans(model), se = TRUE)
})
