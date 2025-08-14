skip_if_not_installed("DCchoice")
withr_library("DCchoice")

test_that("DCchoice package works", {
    data(oohbsyn)
    mod <- oohbchoice(R1 + R2 ~ age + gender | log(BL) + log(BH), data = oohbsyn)

    # weird result seems to make sense manually
    p1 <- transform(oohbsyn, BH = 5)
    p2 <- transform(oohbsyn, BH = 7)
    p1 <- predict(mod, newdata = p1)
    p2 <- predict(mod, newdata = p2)
    bh_comparison <- p2 - p1
    expect_true(all(bh_comparison == 0))

    slo <- avg_slopes(mod)
    pre <- predictions(mod, by = "gender")
    cmp <- comparisons(mod)

    expect_s3_class(cmp, "comparisons")
    expect_s3_class(pre, "predictions")
    expect_s3_class(slo, "slopes")
})

