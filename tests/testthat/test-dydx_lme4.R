skip_if_not_installed("lme4")

library("margins")

test_that("lme4: meffects vs. margins", {

    set.seed(1024)
    N <- 1000
    tmp <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      y = sample(0:1, N, replace = TRUE),
                      group = sample(letters[1:10], N, replace = TRUE))
    mod <- lme4::glmer(y ~ x1 + x2 + (1 | group), data = tmp, family = binomial)
    res <- meffects(mod, variance = NULL)
    mar <- margins(mod)
    meffects:::test_against_margins(res, mar)

    expect_warning(meffects(mod), regexp = "Variance.*not yet supported")

    # TODO: not sure why I get different results
    N <- 1000
    tmp <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      group = sample(letters[1:10], N, replace = TRUE))
    tmp$y <- tmp$x1 + tmp$x2 + tmp$x1 * tmp$x2 + as.numeric(as.factor(tmp$group)) + rnorm(N)
    mod <- lme4::lmer(y ~ x1 + x2 + (1 | group), data = tmp)
    res <- meffects(mod, variance = NULL)
    mar <- margins(mod)
    meffects:::test_against_margins(res, mar)

    # Unsupported arguments
    expect_warning(meffects(mod), regexp = "Variance.*not yet supported")
})
