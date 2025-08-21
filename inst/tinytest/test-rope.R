source("helpers.R")
requiet("rstanarm")
requiet("bayestestR")


dat <- transform(mtcars, cyl = factor(cyl))
mod <- stan_glm(mpg ~ hp + cyl, data = mtcars)

lo <- 21
hi <- 26
p1 <- avg_predictions(mod, by = "cyl", equivalence = c(lo, hi))
post <- get_draws(p1, "DxP")

## Option 1: $Pr(\theta \in ROPE)$
p2 <- apply(post, 2, function(v) {
    mean(lo <= v & v <= hi)
})
p3 <- bayestestR::p_rope(p1, range = c(lo, hi))$p_ROPE

## Option 2: $Pr(\theta \in ROPE | \theta \in CI)$ This is Kruschke's CI+ROPE rule.
p4 <- apply(post, 2, function(v) {
    # 1. Find CI bounds
    CI <- bayestestR::eti(v, ci = 0.95)
    # 2. For the values within the CI bounds...
    in_CI <- CI$CI_low <= v & v <= CI$CI_high
    v_CI <- v[in_CI]
    # 3. What is the conditional probability that they are in ROPE:
    mean(lo <= v_CI & v_CI <= hi)
})
p5 <- rope(p1, range = c(lo, hi), ci = 0.95, ci_method = "ETI")$ROPE_Percentage

expect_equivalent(p1$p.rope.unconditional, p2)
expect_equivalent(p1$p.rope.unconditional, p3)
expect_equivalent(p1$p.rope.conditional, p4)
expect_equivalent(p1$p.rope.conditional, p5)
