mod <- glm(vs ~ hp * mpg, data = mtcars, family = binomial)
mfx <- marginaleffects(mod)
pred <- predictions(mod)

test_that("tidy.predictions", {
    requiet("MASS")
    mod1 <- glm(vs ~ hp * mpg, data = mtcars, family = binomial)
    # there used to be an interaction in this polr model, but it produced
    # negative variances and NaN standard errors
    mod2 <- polr(factor(gear) ~ hp + mpg, data = mtcars)
    pred1 <- predictions(mod1)
    pred2 <- predictions(mod2, type = "probs")
    ti1 <- tidy(pred1)
    ti2 <- tidy(pred2)
    expect_s3_class(ti1, "data.frame")
    expect_s3_class(ti2, "data.frame")
    expect_equal(nrow(ti1), 1)
    expect_equal(nrow(ti2), 3)

    # Stata comparisons (manually collected)
    mod <- lm(mpg ~ hp + wt, data = mtcars)
    pred <- predictions(mod)
    ti <- tidy(pred)
    expect_equal(ti$estimate, 20.09062, tolerance = .0001)
    # not supported yet
    # expect_equal(ti$std.error, 45.84548, tolerance = .0001)
    # expect_equal(ti$conf.low, 19.15298, tolerance = .0001)
    # expect_equal(ti$conf.high, 21.02827, tolerance = .0001)
})


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
    gl <- glance(mfx)
    expect_equal(dim(glance(mfx)), c(1, 10))
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
