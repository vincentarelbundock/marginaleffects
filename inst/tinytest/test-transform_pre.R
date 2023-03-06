# TODO: CI: See comment in last test for how the intervals are back transformed
source("helpers.R")
if (!EXPENSIVE) exit_file("EXPENSIVE")
using("marginaleffects")

requiet("modelsummary")
tol <- .0001


# manual average contrast
mod <- glm(am ~ vs + mpg, data = mtcars, family = binomial)
cmp1 <- comparisons(
    mod,
    variables = list(vs = 0:1),
    comparison = function(hi, lo) mean(hi - lo))
cmp2 <- comparisons(
    mod,
    variables = list(vs = 0:1))
cmp2 <- tidy(cmp2)
expect_equivalent(cmp1$estimate, cmp2$estimate)
expect_equivalent(cmp1$std.error, cmp2$std.error)




# error when function breaks or returns a bad vector
requiet("survey")
data(nhanes, package = "survey")
dat <- setNames(nhanes, tolower(names(nhanes)))
dat$female <- dat$riagendr == 2
dat$race <- sprintf("race%s", dat$race)
mod <- glm(hi_chol ~ female, data = dat, family = binomial)
expect_error(comparisons(mod, comparison = function(x) rep(1, 1234)),
             pattern = "numeric vector")
expect_error(comparisons(mod, comparison = function(hi, lo) head(hi - lo)),
             pattern = "numeric vector")



# univariate vs. Stata
# known stata results
arr_s <- c(arr.est = 0.94026450, arr.std_err = 0.09584693, arr.ci_l = 0.76998425, arr.ci_h = 1.14820184)
ard_s <- c(ard.est = -0.00996557, ard.std_err = 0.01647135, ard.ci_l = -0.04224882, ard.ci_h = 0.02231767)

acs12 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/openintro/acs12.csv")
acs12$disability <- as.numeric(acs12$disability == "yes")
mod <- glm(disability ~ gender, data = acs12, family = binomial)

ard_r <- comparisons(mod, comparison = function(hi, lo) lo - hi)
arr_r <- comparisons(mod, comparison = function(hi, lo) mean(lo) / mean(hi))

cols <- c("estimate", "std.error", "conf.low", "conf.high")
ard_r <- unlist(tidy(ard_r)[, cols])
arr_r <- unlist(tidy(arr_r)[, cols])

expect_equivalent(arr_r[1:2], arr_s[1:2], tolerance = tol)
expect_equivalent(ard_r[1:2], ard_s[1:2], tolerance = tol)



# multivariate vs. Stata
# known stata values
arr_s <- c(arr.est = 0.80285689, arr.std_err = 0.07496766, arr.ci_l = 0.66858441, arr.ci_h = 0.96409545)
ard_s <- c(ard.est = -0.03544519, ard.std_err = 0.01499735, ard.ci_l = -0.06483945, ard.ci_h = -0.00605093)

acs12 <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/openintro/acs12.csv")
acs12$disability <- as.numeric(acs12$disability == "yes")
mod <- glm(disability ~ gender + race + married + age, data = acs12, family = binomial)

ard_r <- comparisons(mod, variables = "gender", comparison = function(hi, lo) lo - hi)
arr_r <- comparisons(mod, variables = "gender", comparison = function(hi, lo) mean(lo) / mean(hi))

cols <- c("estimate", "std.error", "conf.low", "conf.high")
ard_r <- unlist(tidy(ard_r)[, cols])
arr_r <- unlist(tidy(arr_r)[, cols])

# # Stata not sure how CIs are built in Stata
# critical_t <- qt(0.025, df = 1999)
# arr_r <- tidy(arr_r)
# arr_r$estimate - abs(critical_t) * arr_r$std.error
# arr_r$estimate + abs(critical_t) * arr_r$std.error

expect_equivalent(arr_r[1:2], arr_s[1:2], tolerance = tol)
expect_equivalent(arr_r[1:2], arr_s[1:2], tolerance = tol)



# health insurance vs. Stata
# known stata results
arr_s <- c(arr.est = 1.04786879, arr.std_err = 0.00976999, arr.ci_l = 1.02889386, arr.ci_h = 1.06719366)
ard_s <- c(ard.est = 0.04277614, ard.std_err = 0.00837836, ard.ci_l = 0.02635485, ard.ci_h = 0.05919742)

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/HealthInsurance.csv")
dat$health <- as.factor(dat$health)
mod <- glm(health ~ insurance + gender + ethnicity + married + age,
           data = dat, family = binomial)

# # Stata CI: exp(r(lnARR)-invnorm(0.975)*r(lnARR_se))
# lnarr_r <- comparisons(
#     mod, variables = "insurance",
#     comparison = function(hi, lo) log(mean(hi) / mean(lo)))
# lnarr_r <- tidy(lnarr_r)
# exp(lnarr_r$estimate - qnorm(0.975) * lnarr_r$std.error)
# exp(lnarr_r$estimate - qnorm(0.025) * lnarr_r$std.error)

ard_r <- comparisons(
    mod, variables = "insurance",
    comparison = function(hi, lo) hi - lo)

arr_r <- comparisons(
    mod, variables = "insurance",
    comparison = function(hi, lo) mean(hi) / mean(lo))

cols <- c("estimate", "std.error", "conf.low", "conf.high")
ard_r <- unlist(tidy(ard_r)[, cols])
arr_r <- unlist(tidy(arr_r)[, cols])

expect_equivalent(ard_r[1:2], ard_s[1:2], tolerance = tol)
expect_equivalent(arr_r[1:2], arr_s[1:2], tolerance = tol)

# Using manual back-transformation
cols <- c("estimate", "conf.low", "conf.high")
arr_r <- comparisons(
    mod,
    variables = "insurance",
    comparison = function(hi, lo) log(mean(hi) / mean(lo)))
arr_r <- unlist(tidy(arr_r, transform = exp)[, cols])
expect_equivalent(arr_r, arr_s[c(1, 3, 4)], tolerance = tol)



# bugfix: multiple terms w/ n=1 transform
# the function must be applied to each group if it takes a mean or something similar
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/carData/TitanicSurvival.csv")
dat$survived <- as.factor(dat$survived)
mod <- glm(survived ~ passengerClass + sex, data = dat, family = binomial)
cmp <- tidy(comparisons(mod, comparison = function(hi, lo) mean(hi - lo)))
# bug created duplicate estimates
expect_equivalent(length(unique(cmp$estimate)), nrow(cmp))



# TODO: fix eps to make sure slopes() and comparisons() give same result
# comparison slope vs slopes()
mod <- glm(vs ~ mpg + hp, data = mtcars, family = binomial)
mfx1 <- slopes(mod)
mfx2 <- comparisons(mod, comparison = "dydx")
mfx3 <- slopes(mod, eps = 1e-5)
mfx4 <- comparisons(mod, comparison = "dydx", eps = 1e-5)
expect_equivalent(mfx1$estimate, mfx2$estimate)
expect_equivalent(mfx1$std.error, mfx2$std.error)
expect_equivalent(mfx3$estimate, mfx4$estimate)
expect_equivalent(mfx3$std.error, mfx4$std.error)



# # label ratios: We don't have fancy ratio labels anymore, because  +1 is a
# # better label when we do centering.
# mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
# cmp <- comparisons(mod, comparison = "ratio")
# expect_true(all(grepl("\\/", cmp$contrast)))


rm(list = ls())