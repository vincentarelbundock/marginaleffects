skip_if_not_installed("emmeans")
skip_if_not_installed("broom")
requiet("emmeans")
requiet("broom")

dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)
dat$vs <- as.factor(dat$vs)

test_that("variables_grid errors", {
    mod <- lm(mpg ~ cyl + am + vs + hp, dat)
    expect_error(marginalmeans(mod, variables_grid = "junk"), regexp = "not found")
})

test_that("changing the prediction grid changes marginal means", {
    # remember that the grid is variables + variables_grid
    mod <- lm(mpg ~ cyl + am + vs + hp, dat)
    mm1 <- marginalmeans(mod, variables = "cyl")
    mm2 <- marginalmeans(mod, variables = "cyl", variables_grid = "vs")
    mm3 <- marginalmeans(mod, variables = "cyl", variables_grid = "am")
    expect_false(all(mm1$marginalmean == mm2$marginalmean))
    expect_false(all(mm1$marginalmean == mm3$marginalmean))
    expect_false(all(mm2$marginalmean == mm3$marginalmean))
})

test_that("tidy and glance", {
    mod <- lm(mpg ~ cyl + am + hp, dat)
    me <- marginalmeans(mod)
    ti <- tidy(me)
    gl <- glance(me)
    expect_equal(nrow(gl), 1)
    expect_equal(dim(ti), c(5, 8))
})

test_that("marginalmeans vs. emmeans: poisson link or response", {
    dat <- mtcars
    dat$am <- factor(dat$am)
    dat$cyl <- factor(dat$cyl)
    mod <- glm(gear ~ cyl + am, data = dat, family = poisson)
    # link
    mm <- tidy(marginalmeans(mod, variables = "cyl", type = "link"))
    em <- tidy(emmeans(mod, specs = "cyl"))
    expect_equal(mm$estimate, em$estimate)
    expect_equal(mm$std.error, em$std.error)
    # response
    mm <- tidy(marginalmeans(mod, variables = "cyl", type = "response"))
    em <- tidy(emmeans(mod, specs = "cyl", transform = "response"))
    expect_equal(mm$estimate, em$rate)
    expect_equal(mm$std.error, em$std.error, tolerance = .0001)
})


test_that("simple marginal means", {
    mod <- lm(mpg ~ cyl + am + hp, dat)
    em <- broom::tidy(emmeans::emmeans(mod, "cyl"))
    me <- marginalmeans(mod, variables = "cyl")
    expect_equal(me$marginalmean, em$estimate)
    expect_equal(me$std.error, em$std.error)
    em <- broom::tidy(emmeans::emmeans(mod, "am"))
    me <- marginalmeans(mod, variables = "am")
    expect_equal(me$marginalmean, em$estimate)
    expect_equal(me$std.error, em$std.error)
})

test_that("interactions", {
    # standard errors do not match emmeans
    mod <- lm(mpg ~ cyl * am, dat)
    em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "cyl")))
    me <- suppressWarnings(marginalmeans(mod, variables = "cyl"))
    expect_warning(marginalmeans(mod, variables = "cyl"), regex = "interactions")
    expect_equal(me$marginalmean, em$estimate)
    em <- suppressMessages(broom::tidy(emmeans::emmeans(mod, "am")))
    me <- suppressWarnings(marginalmeans(mod, variables = "am"))
    expect_warning(marginalmeans(mod, variables = "am"), regex = "interactions")
    expect_equal(me$marginalmean, em$estimate)
})

test_that("error: no factor", {
    mod <- lm(hp ~ mpg, mtcars)
    expect_error(marginalmeans(mod), regexp = "was found")
})
