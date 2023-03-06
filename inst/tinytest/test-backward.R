# transform_pre -> comparison
# transform_post -> transform
mod <- glm(vs ~ hp, data = mtcars)
cmp1 <- comparisons(mod, transform_pre = "ratio", transform_post = "exp")
cmp2 <- comparisons(mod, comparison = "ratio", transform = "exp")
expect_equivalent(cmp1, cmp2)
pre1 <- predictions(mod, transform_post = "exp")
pre2 <- predictions(mod, transform = "exp")
expect_equivalent(pre1, pre2)