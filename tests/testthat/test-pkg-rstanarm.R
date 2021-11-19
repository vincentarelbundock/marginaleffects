skip_if_not_installed("rstanarm")
library(rstanarm)

test_that("stan_glm: no validity", {
    void <- capture.output(
      mod <- stan_glm(am ~ hp + mpg * vs, data = mtcars, family = binomial(link = "logit"))
    )
    expect_marginaleffects(mod, se = FALSE)
    expect_predictions(predictions(mod), se = FALSE)
})