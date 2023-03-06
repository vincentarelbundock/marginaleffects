source("helpers.R")
using("marginaleffects")

# exponentiate
acs12 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/openintro/acs12.csv")
acs12$disability <- as.numeric(acs12$disability == "yes")
mod <- glm(disability ~ gender + race + married + age, data = acs12, family = binomial)

cmp1 <- comparisons(
    mod,
    variables = "gender",
    comparison = "lnratioavg")
cmp2 <- comparisons(
    mod,
    variables = "gender",
    comparison = "lnratioavg",
    transform = exp)
expect_equivalent(exp(cmp1$estimate), cmp2$estimate)
expect_equivalent(exp(cmp1$conf.low), cmp2$conf.low)
expect_equivalent(exp(cmp1$conf.high), cmp2$conf.high)

# # argument name deprecation
# # aggregate refactor makes thsi possible again
# expect_warning(tidy(cmp2, transform = exp))
# expect_warning(summary(cmp2, transform = exp))

# # aggregate refactor deprecates trasnsform_avg
# tid1 <- tidy(cmp1)
# tid2 <- tidy(cmp1, transform = exp)
# expect_equivalent(exp(tid1$estimate), tid2$estimate)
# expect_equivalent(exp(tid1$conf.low), tid2$conf.low)
# expect_equivalent(exp(tid1$conf.high), tid2$conf.high)


rm(list = ls())