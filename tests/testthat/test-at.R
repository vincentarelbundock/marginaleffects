test_that("`at` with factors and logical", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    mod <- lm(mpg ~ hp + am + gear, data = tmp)
    res1 <- meffects(mod, 
                     variables = "hp",
                     at = list(hp = c(100, 110),
                               gear = c("3", "4"),
                               am = TRUE))
    res2 <- meffects(mod, 
                     variables = "hp",
                     at = list(hp = c(100, 110),
                               gear = c(3, 4),
                               am = TRUE))
    expect_s3_class(res1, "data.frame")
    expect_equal(dim(res1), c(4, 7))
    expect_equal(res1, res2)
})
