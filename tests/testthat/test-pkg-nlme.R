skip_if_not_installed("nlme")
requiet("nlme")

test_that("marginaleffects: nlme::gls: no validity", {
    model <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
                 correlation = corAR1(form = ~ 1 | Mare))
    mfx <- marginaleffects(model)
    expect_s3_class(mfx, "data.frame")
    expect_false(any(mfx$dydx == 0 |  is.na(mfx$dydx)))
    expect_false(any(mfx$std.error == 0 |  is.na(mfx$std.error)))
})

test_that("predictions: nlme::gls: no validity", {
    model <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time),
                 data = Ovary, correlation = corAR1(form = ~ 1 | Mare))
    pred1 <- predictions(model)
    pred2 <- predictions(model, newdata = head(Ovary))
    expect_predictions(pred1, n_row = 1, se = FALSE)
    expect_predictions(pred2, n_row = 6, se = FALSE)
})

test_that("marginalmeans: nlme::gls: no validity", {
    skip("works interactively")
    tmp <- nlme::Ovary
    tmp$categ <- factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
    mod <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time) + categ,
               data = tmp, correlation = corAR1(form = ~ 1 | Mare))
    mm <- marginalmeans(mod)
    expect_marginalmeans(mm)
})
