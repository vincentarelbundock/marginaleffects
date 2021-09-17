skip_if_not_installed("lme4")

requiet("margins")

set.seed(1024)
N <- 1000

# glm model with simulated data
dat_glm <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      clus = sample(letters[1:10], N, replace = TRUE))
dat_glm$y <- dat_glm$x1 + dat_glm$x2 + dat_glm$x1 * dat_glm$x2 + .1 * as.numeric(as.factor(dat_glm$clus))
dat_glm$y <- rbinom(N, 1, plogis(dat_glm$y))
mod_glm <- lme4::glmer(y ~ x1 + x2 + (1 | clus), data = dat_glm, family = binomial)

# lm model with simulated data
dat_lm <- data.frame(x1 = rnorm(N),
                     x2 = rnorm(N),
                     clus = sample(letters[1:10], N, replace = TRUE))
dat_lm$y <- dat_lm$x1 + dat_lm$x2 + dat_lm$x1 * dat_lm$x2 + .1 * as.numeric(as.factor(dat_lm$clus)) + rnorm(N)
mod_lm <- lme4::lmer(y ~ x1 + x2 + (1 | clus), data = dat_lm)


test_that("lme4: no errors generated (no validity check)", {
    res <- marginaleffects(mod_glm)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(2000, 10))
    res <- marginaleffects(mod_lm)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(2000, 10))
})


test_that("vs. margins (dydx only)", {
    res <- marginaleffects(mod_glm, vcov = FALSE)
    mar <- margins(mod_glm)
    expect_true(test_against_margins(res, mar, tolerance = 1e-4))
    res <- marginaleffects(mod_lm, vcov = FALSE)
    mar <- margins(mod_lm)
    expect_true(test_against_margins(res, mar))
})


test_that("vs. margins (dydx and se)", {
    skip("lme4: margins does not appear to support unit_ses=TRUE")

    set.seed(1024)
    mod <- lme4::glmer(y ~ x1 + x2 + (1 | clus), data = dat_glm, family = binomial)
    res <- marginaleffects(mod)
    mar <- margins(mod)
    expect_true(test_against_margins(res, mar))

    mod <- lme4::lmer(y ~ x1 + x2 + (1 | clus), data = dat_lm)
    res <- marginaleffects(mod)
    mar <- margins(mod)
    expect_true(test_against_margins(res, mar))

    # Unsupported arguments
    expect_warning(marginaleffects(mod), regexp = "Variance.*not yet supported")
})


test_that("'group' cannot be a column name because of conflict with tidy output", {
    set.seed(1024)
    N <- 1000
    tmp <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      y = sample(0:1, N, replace = TRUE),
                      group = sample(letters[1:10], N, replace = TRUE))
    mod <- lme4::glmer(y ~ x1 + x2 + (1 | group), data = tmp, family = binomial)
    expect_error(marginaleffects(mod), regexp = "more descriptive")
})


test_that("sanity check on dpoMatrix", {
    k <- marginaleffects(mod_glm, vcov = stats::vcov(mod_glm))
    expect_s3_class(k, "data.frame")
})
