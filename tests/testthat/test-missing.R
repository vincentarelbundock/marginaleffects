test_that("original data with NAs do not pose problems in glm and lm", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    for (i in seq_along(tmp)) {
        tmp[[i]][sample(1:nrow(tmp), 1)] <- NA
    }

    mod1 <- lm(hp ~ mpg + drat + wt + gear, data = tmp)
    mod2 <- glm(vs ~ mpg + drat + wt + gear, data = tmp, family = binomial)
    expect_s3_class(tidy(slopes(mod1)), "data.frame")
    expect_s3_class(tidy(slopes(mod2)), "data.frame")
})


test_that("newdata with NAs do not pose problems in lm", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    for (i in seq_along(tmp)) {
        tmp[[i]][sample(1:nrow(tmp), 1)] <- NA
    }

    mod <- lm(hp ~ mpg + drat + wt + factor(gear), data = tmp)
    mfx <- slopes(mod, newdata = datagrid(drat = c(NA, 10)))
    expect_s3_class(tidy(mfx), "data.frame")
})
