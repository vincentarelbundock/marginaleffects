library(brms)
library(marginaleffects)
skip_if_not_installed("cmdstanr")
skip_if_not_installed("brms")


void <- capture.output({
    dat <- mtcars
    dat$logic <- as.logical(dat$vs)
    dat$cyl_fac <- as.factor(dat$cyl)
    mod_one <- brm(am ~ hp, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_two <- brm(am ~ mpg + hp, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_factor <- brm(am ~ mpg + cyl_fac, data = dat, family = bernoulli(),
                      backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_factor_formula <- brm(am ~ mpg + factor(cyl), data = dat, family = bernoulli(),
                              backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_int <- brm(am ~ mpg * vs, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_log <- brm(am ~ logic, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
})


test_that("predictions: no validity", {
    # simple
    pred <- predictions(mod_two, newdata = typical(hp = c(100, 120)))
    expect_predictions(pred) 
    expect_equal(dim(attr(pred, "posterior_draws")), c(2, 2000))
    # interaction
    pred <- predictions(mod_int, newdata = typical(mpg = c(20, 25)))
    expect_predictions(pred)
    # factor in data frame
    pred <- predictions(mod_factor, newdata = typical())
    expect_predictions(pred)
})


test_that("marginalmeans vs. emmeans", {
    skip_if_not_installed("emmeans")
    skip_if_not_installed("broom")
    expect_error(marginalmeans(mod_factor, variables = "cyl_fac", type = "link"), regexp = "github.*issues")
    # emmeans::emmeans(mod_factor, specs = ~cyl_fac)
})


test_that("marginaleffects: no validity", {
    expect_marginaleffects(mod_two, se = FALSE)
    expect_marginaleffects(mod_int, se = FALSE)
    expect_marginaleffects(mod_factor, se = FALSE)
    # credible intervals and posterior draws
    tmp <- marginaleffects(mod_factor)
    expect_true("conf.low" %in% colnames(tmp))
    expect_true(all(tmp$dydx > tmp$conf.low))
    expect_true(all(tmp$dydx < tmp$conf.high))
    expect_false(is.null(attr(tmp, "posterior_draws")))
    expect_equal(nrow(attr(tmp, "posterior_draws")), nrow(tmp))
})


test_that("marginaleffects vs. emmeans", {
    skip_if_not_installed("emmeans")
    # NOTE: `emmeans` reports the median draw for `dydx`. We report the mean.

    ## known frequentist example to compare syntax
    # mod_one_freq <- glm(am ~ hp, data = mtcars, family = binomial)
    # marginaleffects(mod_one_freq, newdata = typical(hp = 147), type = "link")
    # emmeans::emtrends(mod_one_freq, specs = ~hp, var = "hp", at = list(hp = 147))

    # one variable: link scale
    mfx1 <- marginaleffects(mod_one, variables = "hp", newdata = typical(hp = 110), type = "link")
    mfx2 <- as.data.frame(emmeans::emtrends(mod_one, ~hp, var = "hp", at = list(hp = 110)))
    mfx1_median <- as.vector(apply(attr(mfx1, "posterior_draws"), 1, median))
    expect_equal(mfx1_median, mfx2$hp.trend)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD)

    ## one variable: response scale
    mfx1 <- marginaleffects(mod_one, variables = "hp", newdata = typical(hp = 110))
    mfx2 <- as.data.frame(emmeans::emtrends(mod_one, ~hp, var = "hp", at = list(hp = 110), transform = "response"))
    mfx1_median <- as.vector(apply(attr(mfx1, "posterior_draws"), 1, median))
    expect_equal(mfx1_median, mfx2$hp.trend, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

    # numeric + factor: numeric variable
    dat <- typical(model = mod_factor, mpg = 25, cyl_fac = 4)
    mfx1 <- marginaleffects(mod_factor, variables = "mpg", newdata = dat, type = "link")
    mfx2 <- as.data.frame(emmeans::emtrends(mod_factor, ~mpg, var = "mpg", at = list(mpg = 25, cyl_fac = 4)))
    mfx1_median <- as.vector(apply(attr(mfx1, "posterior_draws"), 1, median))
    expect_equal(mfx1_median, mfx2$mpg.trend, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)

    # numeric + factor: factor
    dat <- typical(model = mod_factor, mpg = 25, cyl_fac = 4)
    mfx1 <- marginaleffects(mod_factor, variables = "cyl_fac", newdata = dat, type = "link")
    mfx2 <- emmeans::emmeans(mod_factor, ~ cyl_fac, var = "cyl_fac", at = list(mpg = 25))
    mfx2 <- emmeans::contrast(mfx2, method = "revpairwise")
    mfx2 <- data.frame(mfx2)[1:2,]
    mfx1_median <- as.vector(apply(attr(mfx1, "posterior_draws"), 1, median))
    expect_equal(mfx1_median, mfx2$estimate, tolerance = .001)
    expect_equal(mfx1$conf.low, mfx2$lower.HPD, tolerance = .001)
    expect_equal(mfx1$conf.high, mfx2$upper.HPD, tolerance = .001)
})


test_that("plot_cap: no validity", {
  p <- plot_cap(mod_int, condition = c("mpg", "vs"))
  vdiffr::expect_doppelganger("brms logit plot_cap", p)
})


test_that("plot_cap: no validity", {
  p <- plot_cap(mod_int, condition = c("mpg", "vs"))
  vdiffr::expect_doppelganger("brms logit plot_cap", p)
})


test_that("factor in formula", {
    skip("https://github.com/easystats/insight/issues/469")
    # marginaleffects
    expect_marginaleffects(mod_factor_formula, se = FALSE)
    # predictions
    pred <- predictions(mod_factor_formula, newdata = typical())
    expect_predictions(pred)
})


test_that("bugs stay dead: factor indexing for posterior draws", {
    tmp <- predictions(mod_factor, newdata = typical(cyl_fac = 4, mpg = c(10, 20))) 
    expect_error(get_posterior_draws(tmp), NA)
})
