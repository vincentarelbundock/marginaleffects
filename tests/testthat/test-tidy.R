mod <- glm(vs ~ hp * mpg, data = mtcars, family = binomial)
mfx <- meffects(mod) 

test_that("tidy: minimal", {
    ti <- tidy(mfx)
    expect_equal(dim(ti), c(2, 7))
    ti <- tidy(mfx, conf.int = FALSE)
    expect_equal(dim(ti), c(2, 5))
    ti1 <- tidy(mfx, conf.level = .90)
    ti2 <- tidy(mfx, conf.level = .99)
    expect_true(all(ti1$conf.low > ti2$conf.low))
    expect_true(all(ti1$conf.high < ti2$conf.high))
})

test_that("glance: with modelsummary", {
    gl <- glance(mfx)
    expect_equal(dim(mfx), c(1, 8))
})


