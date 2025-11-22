requiet("blme")

# Basic expectation tests
dat_simple <- get_dataset("sleepstudy", "lme4")
mod_simple <- blme::blmer(Reaction ~ Days + (1 | Subject), data = dat_simple)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

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

expect_slopes2(fm5)
expect_slopes2(fm6)
expect_slopes2(mod)
expect_predictions2(fm5)
expect_predictions2(fm5)
expect_predictions2(mod)
