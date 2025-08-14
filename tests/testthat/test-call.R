test_that("recall captures calls to avoid evaluating twice", {
    modd <- lm(mpg ~ hp + factor(gear), data = mtcars)
    cmp1 <- comparisons(modd)
    cmp1 <- tidy(cmp1)
    cmp2 <- tidy(comparisons(modd))[, seq_along(cmp1)]
    cmp3 <- comparisons(modd) |> tidy()
    for (col in c("estimate", "std.error", "p.value", "conf.high")) {
        expect_equal(cmp1[[col]], cmp2[[col]])
        expect_equal(cmp1[[col]], cmp3[[col]])
    }

    suppressWarnings(rm("modd", .GlobalEnv))
    suppressWarnings(rm("modd"))
})

