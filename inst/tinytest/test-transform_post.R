source("helpers.R", local = TRUE)

# exponentiate
acs12 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/openintro/acs12.csv")
acs12$disability <- as.numeric(acs12$disability == "yes")
mod <- glm(disability ~ gender + race + married + age, data = acs12, family = binomial)

cmp1 <- comparisons(
    mod,
    variables = "gender",
    transform_pre = "lnratioavg")
cmp2 <- comparisons(
    mod,
    variables = "gender",
    transform_pre = "lnratioavg",
    transform_post = exp)
expect_equivalent(exp(cmp1$comparison), cmp2$comparison)
expect_equivalent(exp(cmp1$conf.low), cmp2$conf.low)
expect_equivalent(exp(cmp1$conf.high), cmp2$conf.high)

# argument name deprecation
expect_warning(tidy(cmp2, transform_post = exp))
expect_warning(summary(cmp2, transform_post = exp))

tid1 <- tidy(cmp1)
tid2 <- tidy(cmp1, transform_avg = exp)
expect_equivalent(exp(tid1$estimate), tid2$estimate)
expect_equivalent(exp(tid1$conf.low), tid2$conf.low)
expect_equivalent(exp(tid1$conf.high), tid2$conf.high)

# string shortcuts and printout
mod <- lm(mpg ~ hp, mtcars)
cmp <- comparisons(mod, transform_post = "exp")
expect_inherits(cmp, "comparisons")
pri <- capture.output(summary(cmp))
expect_equivalent(sum(grepl(" exp ", pri)), 1)