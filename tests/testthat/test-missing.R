test_that("original data with NAs do not pose problems in glm and lm.", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    for (i in seq_along(tmp)) {
        tmp[[i]][sample(1:nrow(tmp), 1)] <- NA
    }
    mod1 <- lm(hp ~ mpg + drat + wt + factor(gear), data = tmp)
    mod2 <- glm(vs ~ mpg + drat + wt + factor(gear), data = tmp, family = binomial)
    expect_s3_class(tidy(mod1), "data.frame")
    expect_s3_class(tidy(mod2), "data.frame")
})

test_that("newdata with NAs do not pose problems in lm.", {
    mod <- lm(hp ~ mpg + drat + wt + factor(gear), data = tmp)
    nd <- typical(mod, at = list("drat" = c(NA, 10)))
    expect_s3_class(tidy(marginaleffects(mod, newdata = nd)), "data.frame")
})
