library(brms)
library(marginaleffects)
library(insight)
skip_if_not_installed("cmdstanr")
skip_if_not_installed("brms")
skip("brms is not officially supported yet")


test_that("predictions: no validity", {
  void <- capture.output(
    mod <- brm(am ~ mpg + hp, data = mtcars, family = bernoulli(),
               backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
  )
  pred <- predictions(mod, newdata = typical(hp = c(100, 120)))
  expect_equal(pred$predicted, c(0.0443223519583173, 0.119305664275307))
  expect_equal(dim(attr(pred, "posterior_draws")), c(2, 2000))
})


test_that("plot_cap: no validity", {
  skip("TODO: define a test")
  void <- capture.output(
    mod <- brm(am ~ mpg * vs, data = mtcars, family = bernoulli(),
               backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
  )
  p <- plot_cap(mod, condition = c("mpg", "vs"))
})


test_that()
  skip("TODO: define a test")
  void <- capture.output(
      mod <- brm(am ~ mpg * vs, data = mtcars, family = bernoulli(),
                backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
  )


test_that("contrast: no validity", {
  skip("TODO: define a test")
  void <- capture.output(
      mod <- brm(am ~ mpg * vs, data = mtcars, family = bernoulli(),
                backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
  )

pkgload::load_all()
  a = get_contrasts(model = mod, variable = "mpg", newdata = counterfactual(vs = 0:1))
  b = marginaleffects(model = mod, variable = "mpg")

})


  dat <- mtcars
  dat$cyl <- as.factor(dat$cyl)
  void <- capture.output(
    mod <- brm(am ~ mpg + cyl, data = dat, family = bernoulli(),
               backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000))
  marginaleffects(mod) |> summary()


test_that("marginaleffects: factors in formula", {
  void <- capture.output(
    mod <- brm(am ~ mpg + factor(cyl), data = mtcars, family = bernoulli(),
               backend = "cmdstanr", seed = 1024, silent = 2, chains = 4, iter = 1000)
  )
  expect_error(marginaleffects(mod), NA)
})
