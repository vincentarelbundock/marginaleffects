skip("TODO: fix this")

test_that("nlme package works", {
    skip_if_not_installed("nlme")
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")

    withr_library("nlme")
    withr_library("emmeans")
    withr_library("broom")

    dat <- get_dataset("Ovary", "nlme")

    # nlme::gls: marginaleffects vs. emtrends
    model <- gls(follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time), dat, correlation = corAR1(form = ~ 1 | Mare))
    mfx <- slopes(model)
    expect_s3_class(mfx, "data.frame")
    expect_false(any(mfx$estimate == 0 | is.na(mfx$estimate)))
    expect_false(any(mfx$std.error == 0 | is.na(mfx$std.error)))
    # emtrends
    nd <- datagrid(newdata = dat, Time = 1)
    mfx <- slopes(model, variables = "Time", type = "link", newdata = datagrid(Time = 1))
    em <- suppressMessages(emtrends(model, ~Time, "Time", mode = "df.error", at = list(Time = 1)))
    em <- tidy(em)
    expect_equal(mfx$std.error, em$std.error, tolerance = .001, ignore_attr = TRUE)
    expect_equal(mfx$estimate, em$Time.trend, tolerance = .01, ignore_attr = TRUE)

    # predictions: nlme::gls: no validity
    model <- gls(
        follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time),
        data = dat,
        correlation = corAR1(form = ~ 1 | Mare)
    )
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(dat))
    expect_s3_class(pred1, "predictions")
    expect_equal(nrow(pred1), nrow(dat), ignore_attr = TRUE)
    expect_s3_class(pred2, "predictions")
    expect_equal(nrow(pred2), 6, ignore_attr = TRUE)

    # glm: marginalmeans vs emmeans
    tmp <- dat
    tmp$categ <- factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
    mod <- gls(
        follicles ~ sin(2 * pi * Time) + cos(2 * pi * Time) + categ,
        data = tmp,
        correlation = corAR1(form = ~ 1 | Mare)
    )
    em <- suppressMessages(emmeans(mod, specs = "categ"))
    em <- tidy(em)
    mm <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = "categ") |> dplyr::arrange(categ)
    expect_equal(mm$estimate, em$estimate, ignore_attr = TRUE)
    expect_equal(mm$std.error, em$std.error, tolerance = 1e-5, ignore_attr = TRUE)

    # issue #99: Support `lme`
    mod <- lme(distance ~ age + Sex, data = Orthodont, random = ~1)
    mfx <- avg_slopes(mod)
    cmp <- comparisons(mod)
    pre <- predictions(mod)
    expect_s3_class(mfx, "slopes")
    expect_s3_class(cmp, "comparisons")
    expect_s3_class(pre, "predictions")
})
