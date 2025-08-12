source("helpers.R")
using("marginaleffects")

# G-computation tests with mtcars
mod <- glm(am ~ vs * wt, data = mtcars, family = binomial)
d0 <- transform(mtcars, vs = 0) # Everyone untreated
d1 <- transform(mtcars, vs = 1) # Everyone treated
p0 <- predictions(mod, newdata = d0, type = "response", vcov = FALSE)$estimate
p1 <- predictions(mod, newdata = d1, type = "response", vcov = FALSE)$estimate
rd_manu <- mean(p1) - mean(p0)
rr_manu <- mean(p1) / mean(p0)
rd_auto <- avg_comparisons(mod, variables = "vs", comparison = "difference")$estimate
rr_auto <- avg_comparisons(mod, variables = "vs", comparison = "ratio")$estimate
expect_equivalent(rd_manu, rd_auto)
expect_equivalent(rr_manu, rr_auto)
