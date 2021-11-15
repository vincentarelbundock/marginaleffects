library(brms)
library(marginaleffects)
library(insight)
skip_if_not_installed("cmdstanr")
skip_if_not_installed("brms")


void <- capture.output({
    dat <- mtcars
    dat$cyl_fac <- as.factor(dat$cyl)
    mod_simple <- brm(am ~ mpg + hp, data = dat, family = bernoulli(),
                      backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_factor <- brm(am ~ mpg + cyl_fac, data = dat, family = bernoulli(),
                      backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_factor_formula <- brm(am ~ mpg + factor(cyl), data = dat, family = bernoulli(),
                              backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
    mod_int <- brm(am ~ mpg * vs, data = dat, family = bernoulli(),
                   backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
})


test_that("marginaleffects: no validity", {
    expect_marginaleffects(mod_simple, se = FALSE)
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


test_that("predictions: no validity", {
    # simple
    pred <- predictions(mod_simple, newdata = typical(hp = c(100, 120)))
    expect_predictions(pred) 
    expect_equal(pred$predicted, c(0.0443223519583173, 0.119305664275307))
    expect_equal(dim(attr(pred, "posterior_draws")), c(2, 2000))
    # interaction
    pred <- predictions(mod_int, newdata = typical(mpg = c(20, 25)))
    expect_predictions(pred)
    # factor in data frame
    pred <- predictions(mod_factor, newdata = typical())
    expect_predictions(pred)
})


test_that("plot_cap: no validity", {
  p <- plot_cap(mod_int, condition = c("mpg", "vs"))
  vdiffr::expect_doppelganger("brms logit plot_cap", p)
})


test_that("contrast: no validity", {
  skip("TODO: define a test")
  # a = get_contrasts(model = mod, variable = "mpg", newdata = counterfactual(vs = 0:1))
  # b = marginaleffects(model = mod, variable = "mpg")
})


test_that("factor in formula", {
    skip("https://github.com/easystats/insight/issues/469")
    # marginaleffects
    expect_marginaleffects(mod_factor_formula, se = FALSE)
    # predictions
    pred <- predictions(mod_factor_formula, newdata = typical())
    expect_predictions(pred)
})
