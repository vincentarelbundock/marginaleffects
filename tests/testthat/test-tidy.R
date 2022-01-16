mod <- glm(vs ~ hp * mpg, data = mtcars, family = binomial)
mfx <- marginaleffects(mod)


test_that("tidy: minimal", {
    ti <- tidy(mfx)
    expect_equal(dim(ti), c(2, 8))
    ti <- tidy(mfx, conf.int = FALSE)
    expect_equal(dim(ti), c(2, 6))
    ti1 <- tidy(mfx, conf.level = .90)
    ti2 <- tidy(mfx, conf.level = .99)
    expect_true(all(ti1$conf.low > ti2$conf.low))
    expect_true(all(ti1$conf.high < ti2$conf.high))
})

test_that("glance: with modelsummary", {
    skip_if_not_installed("modelsummary", minimum_version = "0.9.3")
    gl <- glance(mfx)
    expect_equal(dim(glance(mfx)), c(1, 9))
})

test_that("bug: emmeans contrast rename in binomial", {
    x <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
    x <- marginaleffects(x)
    x <- tidy(x)
    expect_s3_class(x, "data.frame") 
    expect_equal(nrow(x), 3)
})

test_that("tidy: with and without contrasts", {
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)

    # numeric only
    x <- tidy(marginaleffects(lm(mpg ~ hp, tmp)))
    expect_equal(dim(x), c(1, 8))

    # logical only
    model <- lm(mpg ~ am, tmp)
    x <- tidy(marginaleffects(model))
    expect_equal(dim(x), c(1, 9))

    # factor only
    model <- lm(mpg ~ factor(gear), tmp)
    x <- tidy(marginaleffects(model))
    expect_equal(dim(x), c(2, 9))

    # combinations
    x <- tidy(marginaleffects(lm(mpg ~ hp + am, tmp)))
    expect_equal(dim(x), c(2, 9))

    x <- tidy(marginaleffects(lm(mpg ~ hp + factor(gear), tmp)))
    expect_equal(dim(x), c(3, 9))

    x <- tidy(marginaleffects(lm(mpg ~ am + factor(gear), tmp)))
    expect_equal(dim(x), c(3, 9))

    x <- tidy(marginaleffects(lm(mpg ~ hp + am + factor(gear), tmp)))
    expect_equal(dim(x), c(4, 9))
})


test_that("bugs stay dead: multi-type are not duplicated", {
    mod <- glm(am ~ mpg, family = binomial, data = mtcars)
    mfx <- marginaleffects(mod, type = c("response", "link"))
    ti <- tidy(mfx)
    expect_equal(nrow(ti), 2)
})
