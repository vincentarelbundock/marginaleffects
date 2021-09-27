skip_if_not_installed("emmeans")
skip_if_not_installed("broom")


dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)


test_that("tidy and glance", {
    mod <- lm(mpg ~ cyl + am + hp, dat)
    me <- marginalmeans(mod)
    ti <- tidy(me)
    gl <- glance(me)
    expect_equal(nrow(gl), 1)
    expect_equal(dim(ti), c(5, 8))
})


test_that("simple marginal means", {
    mod <- lm(mpg ~ cyl + am + hp, dat)
    em <- broom::tidy(emmeans::emmeans(mod, "cyl"))
    me <- marginalmeans(mod, variables = "cyl")
    expect_equal(me$predicted, em$estimate)
    expect_equal(me$std.error, em$std.error)
    em <- broom::tidy(emmeans::emmeans(mod, "am"))
    me <- marginalmeans(mod, variables = "am")
    expect_equal(me$predicted, em$estimate)
    expect_equal(me$std.error, em$std.error)
})


test_that("interactions", {
    # standard errors do not match emmeans
    mod <- lm(mpg ~ cyl * am, dat)
    em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "cyl")))
    me <- suppressWarnings(marginalmeans(mod, variables = "cyl"))
    expect_warning(marginalmeans(mod, variables = "cyl"), regex = "interactions")
    expect_equal(me$predicted, em$estimate)
    em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "am")))
    me <- suppressWarnings(marginalmeans(mod, variables = "am"))
    expect_warning(marginalmeans(mod, variables = "am"), regex = "interactions")
    expect_equal(me$predicted, em$estimate)
})


test_that("error: no factor", {
    mod <- lm(hp ~ mpg, mtcars)
    expect_error(marginalmeans(mod), regexp = "was found")
})
