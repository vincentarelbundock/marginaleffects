test_that("estimatr package works", {
    skip_if_not_installed("estimatr")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("margins")
    skip_if_not_installed("broom")

    withr_library("estimatr")
    withr_library("emmeans")
    withr_library("margins")
    withr_library("broom")

    Km <- get_dataset("Kmenta", "sem")
    dat <- transform(mtcars, cyl = factor(cyl))

    # lm_lin: no validity
    mod <- lm_lin(mpg ~ am, ~ hp + cyl, data = dat)
    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")
    slo <- slopes(mod)
    expect_s3_class(slo, "slopes")

    # iv_robust vs. stata
    stata <- readRDS(test_path("stata/stata.rds"))$estimatr_iv_robust
    model <- iv_robust(
        Q ~ P + D | D + F + A,
        se_type = "stata",
        data = Km
    )
    mfx <- slopes(model, newdata = Km)
    tid <- tidy(mfx)
    mfx <- merge(tid, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1, ignore_attr = TRUE)

    # lm_robust vs. stata vs. emtrends
    model <- lm_robust(carb ~ wt + factor(cyl), se_type = "HC2", data = dat)
    stata <- readRDS(test_path("stata/stata.rds"))$estimatr_lm_robust
    mfx <- tidy(slopes(model))
    mfx$term <- ifelse(mfx$contrast == "6 - 4", "6.cyl", mfx$term)
    mfx$term <- ifelse(mfx$contrast == "8 - 4", "8.cyl", mfx$term)
    mfx <- merge(mfx, stata)
    expect_equal(mfx$estimate, mfx$dydxstata, ignore_attr = TRUE)
    expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .1, ignore_attr = TRUE)
    # emtrends
    mfx <- slopes(model, newdata = datagrid(cyl = 4, wt = 2, newdata = dat), variables = "wt")
    em <- emtrends(model, ~wt, "wt", at = list(cyl = 4, wt = 2))
    em <- tidy(em)
    expect_equal(mfx$estimate, em$wt.trend, tolerance = .001, ignore_attr = TRUE)
    expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)
    # margins does not support standard errors
    tmp <- mtcars
    tmp$cyl <- factor(tmp$cyl)
    mod <- lm_robust(carb ~ wt + cyl, data = tmp, se_type = "stata")
    mar <- margins(mod, data = head(tmp))
    mfx <- slopes(mod, newdata = head(tmp))
    expect_true(all(abs(mfx$estimate - mar$dydx) < 0.1))

    # iv_robust: predictions: no validity
    # skip_if_not_installed("insight", minimum_version = "0.17.1")
    model <- iv_robust(Q ~ P + D | D + F + A, se_type = "stata", data = Km)
    pre <- predictions(model, newdata = Km)
    expect_s3_class(pre, "predictions")
    expect_equal(nrow(pre), nrow(Km), ignore_attr = TRUE)
    pre <- predictions(model, newdata = head(Km))
    expect_s3_class(pre, "predictions")
    expect_equal(nrow(pre), 6, ignore_attr = TRUE)

    # lm_robust: marginalmeans predictions: no validity
    # skip_if_not_installed("insight", minimum_version = "0.17.1")
    tmp <- mtcars
    tmp$cyl <- as.factor(tmp$cyl)
    tmp$am <- as.logical(tmp$am)
    model <- lm_robust(carb ~ wt + am + cyl, se_type = "stata", data = tmp)
    pre <- predictions(model)
    expect_s3_class(pre, "predictions")
    expect_equal(nrow(pre), nrow(tmp), ignore_attr = TRUE)
    pre <- predictions(model, newdata = head(tmp))
    expect_s3_class(pre, "predictions")
    expect_equal(nrow(pre), 6, ignore_attr = TRUE)
})
