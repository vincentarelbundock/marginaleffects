source("helpers.R")
using("marginaleffects")
requiet("blme")

# exit_file("TODO: does not error consistently")

dat <- get_dataset("sleepstudy", "lme4")
penaltyFn <- function(sigma) dcauchy(sigma, 0, 10, log = TRUE)
fm5 <- blmer(
    Reaction ~ Days + (0 + Days | Subject),
    data = dat,
    cov.prior = custom(penaltyFn, chol = TRUE, scale = "log")
)
fm6 <- blmer(
    Reaction ~ Days + (1 + Days | Subject),
    data = dat,
    cov.prior = NULL,
    fixef.prior = normal
)
mod <- bglmer(vs ~ mpg + (1 | gear), data = mtcars, family = binomial)

expect_slopes(fm5)
expect_slopes(fm6)
expect_slopes(mod)

# suppressWarnings about standard errors not account for uncertainty in random effects
pre <- predictions(fm5) |> suppressWarnings()
expect_predictions(pre)
pre <- predictions(fm6) |> suppressWarnings()
expect_predictions(pre)
pre <- predictions(mod) |> suppressWarnings()
expect_predictions(pre)
