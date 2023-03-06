source("helpers.R")
using("marginaleffects")

requiet("pscl")

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp)


# Issue #580: binary outcome should not be included in marginalmeans calculation
dat <- transform(
    mtcars,
    vs = factor(vs),
    gear = factor(gear),
    am = factor(am))
mod <- glm(vs ~ gear + am, data = dat, family = binomial)
p <- predictions(mod, newdata = "marginalmeans")
expect_equal(nrow(p), 6)


# bugfix: counterfactual predictions keep rowid
mod <- lm(mpg ~ hp + am, mtcars)
pred <- predictions(mod, newdata = datagrid(am = 0:1, grid_type = "counterfactual"))
expect_predictions(pred, n_row = 64)
expect_true("rowidcf" %in% colnames(pred))


# `variables` argument: character vector
p <- predictions(mod, variables = list("am" = 0:1))
expect_equivalent(nrow(p), 64)

p <- predictions(mod, variables = list("am" = 0:1), newdata = "mean")
expect_equivalent(nrow(p), 2)

# `variables` argument: character vector
expect_error(predictions(mod, variables = list(2)), pattern = "names")
p <- predictions(mod, variables = "am")
expect_inherits(p, "predictions")


# average prediction with delta method are asymptotically equivalent to back transformed
set.seed(1024)
N <- 1e5
dat <- data.frame(
    y = rbinom(N, 1, prob = .9),
    x = rnorm(N))
mod <- glm(y ~ x, family = binomial, data = dat)
p1 <- tidy(predictions(mod)) # average prediction outside [0,1]
p2 <- tidy(predictions(mod, type = "link"), transform = insight::link_inverse(mod)) # average prediction inside [0,1]
expect_equivalent(p1$estimate, p2$estimate, tolerance = .001)
expect_equivalent(p1$conf.low, p2$conf.low, tolerance = .01)
expect_equivalent(p1$conf.high, p2$conf.high, tolerance = .01)


################
#  conf.level  #
################

# conf.level argument changes width of interval
mod <- lm(mpg ~ hp + am, mtcars)
for (L in c(.4, .7, .9, .95, .99, .999)) {
    nd <- datagrid(model = mod)
    unknown <- predictions(mod, newdata = nd, conf.level = L, df = insight::get_df(mod)) # known values used Wald
    known <- predict(mod, newdata = nd, se.fit = TRUE, interval = "confidence", level = L)$fit
    expect_equivalent(unknown$conf.low, known[, "lwr"])
    expect_equivalent(unknown$conf.high, known[, "upr"])
}


#################################
#  average adjusted predictions #
#################################
dat <- mtcars
dat$w <- 1:32
mod <- lm(mpg ~ hp + am, dat)
pre1 <- predictions(mod, by = "am")
pre1 <- pre1[order(pre1$am),]
pre2 <- predictions(mod)
pre2 <- aggregate(estimate ~ am, FUN = mean, data = pre2)
expect_equivalent(pre1$estimate, pre2$estimate)


#########################################
#  weigted average adjusted predictions #
#########################################
pre1 <- predictions(mod, wts = mtcars$w)
pre2 <- predictions(mod)
tid1 <- tidy(pre1)
tid2 <- tidy(pre2)
expect_equivalent(pre1$estimate, pre2$estimate)
expect_true(all(tid1$estimate != tid2$estimate))



######################################
#  values against predict benchmark  #
######################################
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp)
nd <- datagrid(model = mod, cyl = c(4, 6, 8))
mm <- predictions(mod, newdata = nd)
expect_equivalent(mm$estimate, unname(predict(mod, newdata = nd)))





#############################
#  size: new data argument  #
#############################

# `newdata`: mtcars has 32 rows
mm <- predictions(mod, newdata = tmp)
expect_equivalent(nrow(mm), 32)


# `typical`: all factors
mm <- predictions(mod, newdata = datagrid(cyl = c(4, 6, 8)))
expect_equivalent(nrow(mm), 3)


# `typical`: two missing factors
mm <- predictions(mod, newdata = datagrid(cyl = 4))
expect_equivalent(nrow(mm), 1)


# `typical`: one missing factor
mm <- predictions(mod, newdata = datagrid(cyl = c(4, 6)))
expect_equivalent(nrow(mm), 2)


# `typical`: all logical
mm <- predictions(mod, newdata = datagrid(am = c(TRUE, FALSE)))
expect_equivalent(nrow(mm), 2)
expect_equivalent(length(unique(mm$estimate)), nrow(mm))


# `typical`: missing logical
mm <- predictions(mod, newdata = datagrid(am = TRUE))
expect_equivalent(nrow(mm), 1)






exit_file("works interactively")


# Issue #496
mod <- lm(mpg ~ factor(vs), data = mtcars)
p1 <- predictions(mod, variables = list(vs = 0:1))
p2 <- predictions(mod, variables = list(vs = c("0", "1")))
expect_inherits(p1, "predictions")
expect_inherits(p2, "predictions")
expect_error(predictions(mod, variables = list(vs = "pairwise")), pattern = "pairwise")

dat <- mtcars
dat$vs <- factor(dat$vs)
mod <- lm(mpg ~ vs, data = dat)
p1 <- predictions(mod, variables = list(vs = 0:1))
p2 <- predictions(mod, variables = list(vs = c("0", "1")))
expect_inherits(p1, "predictions")
expect_inherits(p2, "predictions")
expect_error(predictions(mod, variables = list(vs = "pairwise")), pattern = "pairwise")


#########################################################################
#  some models do not return data.frame under `insight::get_predicted`  #
#########################################################################

# Issue 514
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/MatchIt/lalonde.csv")

fit <- lm(re78 ~ married + race + age, data = dat)

p <- predictions(fit, variables = list(age = c(20, 30)), newdata = dat)
expect_equivalent(nrow(p), nrow(dat) * 2)

p <- predictions(fit, variables = list(age = c(20, 25, 30)), newdata = dat)
expect_equivalent(nrow(p), nrow(dat) * 3)

p <- predictions(fit, variables = list(age = "minmax"), newdata = dat)
expect_equivalent(nrow(p), nrow(dat) * 2)

p <- predictions(fit, variables = list(race = c("black", "hispan")), newdata = dat)
expect_equivalent(nrow(p), nrow(dat) * 2)

p <- predictions(fit, variables = list(race = c("black", "hispan", "white")), newdata = dat)
expect_equivalent(nrow(p), nrow(dat) * 3)

expect_error(predictions(fit, variables = list(race = "all"), newdata = dat), pattern = "Check")

p <- predictions(fit, newdata = datagridcf(race = c("black", "hispan", "white")))
expect_equivalent(nrow(p), nrow(dat) * 3)

dat <- transform(mtcars, am = as.logical(am))
mod <- lm(mpg ~ am, dat)

p <- predictions(mod, variables = list("am" = TRUE), newdata = dat)
expect_equivalent(nrow(p), 32)

p <- predictions(mod, variables = "am", newdata = dat)
expect_equivalent(nrow(p), 64)


# hurdle predictions
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/pscl/bioChemists.csv")
mod <- hurdle(art ~ phd + fem | ment, data = dat, dist = "negbin")
pred <- predictions(mod, newdata = dat)
expect_inherits(pred, "data.frame")
expect_true("estimate" %in% colnames(pred))


# Issue #655: Average counterfactual predictions
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
pre <- avg_predictions(mod, variables = "cyl")
expect_inherits(pre, "predictions")
expect_equivalent(nrow(pre), 3)
pre <- avg_predictions(mod, variables = list(cyl = c(4, 6)))
expect_inherits(pre, "predictions")
expect_equivalent(nrow(pre), 2)
pre <- avg_predictions(mod, by = "cyl", newdata = datagridcf(cyl = c(4, 6)))
expect_inherits(pre, "predictions")
expect_equivalent(nrow(pre), 2)


rm(list = ls())