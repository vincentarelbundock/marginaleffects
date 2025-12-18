testthat::skip_if_not_installed("pscl")
requiet("pscl")


tmp_predictions <- mtcars
tmp_predictions$am <- as.logical(tmp_predictions$am)
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp_predictions)


# Issue #580: binary outcome should not be included in marginalmeans calculation
dat_predictions <- transform(
    mtcars,
    vs = factor(vs),
    gear = factor(gear),
    am = factor(am)
)
mod <- glm(vs ~ gear + am, data = dat_predictions, family = binomial)
p <- predictions(mod, newdata = "balanced")
expect_equal(nrow(p), 6)


# bugfix: counterfactual predictions keep rowid
mod <- lm(mpg ~ hp + am, mtcars)
pred <- predictions(mod, newdata = datagrid(am = 0:1, grid_type = "counterfactual"))
expect_predictions2(mod, newdata = datagrid(am = 0:1, grid_type = "counterfactual"), n_row = 64)
expect_true("rowidcf" %in% colnames(pred))


# `variables` argument: character vector
p <- predictions(mod, variables = list("am" = 0:1))
expect_equal(nrow(p), 64, ignore_attr = TRUE)

p <- predictions(mod, variables = list("am" = 0:1), newdata = "mean")
expect_equal(nrow(p), 2, ignore_attr = TRUE)

# `variables` argument: character vector
expect_error(predictions(mod, variables = list(2)), regexp = "names")
p <- predictions(mod, variables = "am")
expect_s3_class(p, "predictions")


# average prediction with delta method are asymptotically equivalent to back transformed
set.seed(1024)
N <- 1e5
dat_predictions2 <- data.frame(
    y = rbinom(N, 1, prob = .9),
    x = rnorm(N)
)
mod <- glm(y ~ x, family = binomial, data = dat_predictions2)
p1 <- avg_predictions(mod) # average prediction outside [0,1]
p2 <- avg_predictions(mod, type = "link", transform = insight::link_inverse(mod)) # average prediction inside [0,1]
expect_equal(p1$estimate, p2$estimate, tolerance = .001, ignore_attr = TRUE)
expect_equal(p1$conf.low, p2$conf.low, tolerance = .01, ignore_attr = TRUE)
expect_equal(p1$conf.high, p2$conf.high, tolerance = .01, ignore_attr = TRUE)


################
#  conf.level  #
################

# conf.level argument changes width of interval
mod <- lm(mpg ~ hp + am, mtcars)
for (L in c(.4, .7, .9, .95, .99, .999)) {
    nd <- datagrid(model = mod)
    unknown <- predictions(mod, newdata = nd, conf.level = L, df = insight::get_df(mod)) # known values used Wald
    known <- predict(mod, newdata = nd, se.fit = TRUE, interval = "confidence", level = L)$fit
    expect_equal(unknown$conf.low, known[, "lwr"], tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(unknown$conf.high, known[, "upr"], tolerance = 1e-5, ignore_attr = TRUE)
}


#################################
#  average adjusted predictions #
#################################
dat_predictions3 <- mtcars
dat_predictions3$w <- 1:32
mod <- lm(mpg ~ hp + am, dat_predictions3)
pre1 <- predictions(mod, by = "am")
pre1 <- pre1[order(pre1$am), ]
pre2 <- predictions(mod)
pre2 <- aggregate(estimate ~ am, FUN = mean, data = pre2)
expect_equal(pre1$estimate, pre2$estimate, tolerance = 1e-6, ignore_attr = TRUE)


#########################################
#  weigted average adjusted predictions #
#########################################
pre1 <- avg_predictions(mod, wts = mtcars$w)
pre2 <- avg_predictions(mod)
expect_true(all(pre1$estimate != pre2$estimate))


######################################
#  values against predict benchmark  #
######################################
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp_predictions)
nd <- datagrid(model = mod, cyl = c(4, 6, 8))
mm <- predictions(mod, newdata = nd)
expect_equal(mm$estimate, unname(predict(mod, newdata = nd)), tolerance = 1e-6, ignore_attr = TRUE)


#############################
#  size: new data argument  #
#############################

# `newdata`: mtcars has 32 rows
mm <- predictions(mod, newdata = tmp_predictions)
expect_equal(nrow(mm), 32, ignore_attr = TRUE)


# `typical`: all factors
mm <- predictions(mod, newdata = datagrid(cyl = c(4, 6, 8)))
expect_equal(nrow(mm), 3, ignore_attr = TRUE)


# `typical`: two missing factors
mm <- predictions(mod, newdata = datagrid(cyl = 4))
expect_equal(nrow(mm), 1, ignore_attr = TRUE)


# `typical`: one missing factor
mm <- predictions(mod, newdata = datagrid(cyl = c(4, 6)))
expect_equal(nrow(mm), 2, ignore_attr = TRUE)


# `typical`: all logical
mm <- predictions(mod, newdata = datagrid(am = c(TRUE, FALSE)))
expect_equal(nrow(mm), 2, ignore_attr = TRUE)
expect_equal(length(unique(mm$estimate)), nrow(mm), ignore_attr = TRUE)


# `typical`: missing logical
mm <- predictions(mod, newdata = datagrid(am = TRUE))
expect_equal(nrow(mm), 1, ignore_attr = TRUE)


# Issue #496
mod <- lm(mpg ~ factor(vs), data = mtcars)
p1 <- predictions(mod, variables = list(vs = 0:1))
p2 <- predictions(mod, variables = list(vs = c("0", "1")))
expect_s3_class(p1, "predictions")
expect_s3_class(p2, "predictions")
expect_error(predictions(mod, variables = list(vs = "pairwise")), regexp = "pairwise")

dat_predictions4 <- mtcars
dat_predictions4$vs <- factor(dat_predictions4$vs)
mod <- lm(mpg ~ vs, data = dat_predictions4)
p1 <- predictions(mod, variables = list(vs = 0:1))
p2 <- predictions(mod, variables = list(vs = c("0", "1")))
expect_s3_class(p1, "predictions")
expect_s3_class(p2, "predictions")
expect_error(predictions(mod, variables = list(vs = "pairwise")), regexp = "pairwise")


#########################################################################
#  some models do not return data.frame under `insight::get_predicted`  #
#########################################################################

# Issue 514
dat_predictions5 <- get_dataset("lalonde", "MatchIt")

fit <- lm(re78 ~ married + race + age, data = dat_predictions5)

p <- predictions(fit, variables = list(age = c(20, 30)), newdata = dat_predictions5)
expect_equal(nrow(p), nrow(dat_predictions5) * 2, ignore_attr = TRUE)

p <- predictions(fit, variables = list(age = c(20, 25, 30)), newdata = dat_predictions5)
expect_equal(nrow(p), nrow(dat_predictions5) * 3, ignore_attr = TRUE)

p <- predictions(fit, variables = list(age = "minmax"), newdata = dat_predictions5)
expect_equal(nrow(p), nrow(dat_predictions5) * 2, ignore_attr = TRUE)

p <- predictions(fit, variables = list(race = c("black", "hispan")), newdata = dat_predictions5)
expect_equal(nrow(p), nrow(dat_predictions5) * 2, ignore_attr = TRUE)

p <- predictions(fit, variables = list(race = c("black", "hispan", "white")), newdata = dat_predictions5)
expect_equal(nrow(p), nrow(dat_predictions5) * 3, ignore_attr = TRUE)

expect_error(predictions(fit, variables = list(race = "all"), newdata = dat_predictions5), regexp = "Check")

p <- predictions(
    fit,
    newdata = datagrid(
        race = c("black", "hispan", "white"),
        grid_type = "counterfactual"
    )
)
expect_equal(nrow(p), nrow(dat_predictions5) * 3, ignore_attr = TRUE)

dat_predictions6 <- transform(mtcars, am = as.logical(am))
mod <- lm(mpg ~ am, dat_predictions6)

p <- predictions(mod, variables = list("am" = TRUE), newdata = dat_predictions6)
expect_equal(nrow(p), 32, ignore_attr = TRUE)

p <- predictions(mod, variables = "am", newdata = dat_predictions6)
expect_equal(nrow(p), 64, ignore_attr = TRUE)


# hurdle predictions
dat_predictions7 <- get_dataset("bioChemists", "pscl")
mod <- hurdle(art ~ phd + fem | ment, data = dat_predictions7, dist = "negbin")
pred <- predictions(mod, newdata = dat_predictions7)
expect_s3_class(pred, "data.frame")
expect_true("estimate" %in% colnames(pred))


# Issue #655: Average counterfactual predictions
mod <- lm(mpg ~ hp + factor(cyl), data = mtcars)
pre <- avg_predictions(mod, variables = "cyl")
expect_s3_class(pre, "predictions")
expect_equal(nrow(pre), 3, ignore_attr = TRUE)
pre <- avg_predictions(mod, variables = list(cyl = c(4, 6)))
expect_s3_class(pre, "predictions")
expect_equal(nrow(pre), 2, tolerance = 1e-6, ignore_attr = TRUE)
pre <- avg_predictions(mod, by = "cyl", newdata = datagrid(cyl = c(4, 6), grid_type = "counterfactual"))
expect_s3_class(pre, "predictions")
expect_equal(nrow(pre), 2, tolerance = 1e-6, ignore_attr = TRUE)


# Issue #994: averaging linkinv(link)
mod <- glm(vs ~ mpg + cyl, data = mtcars, family = binomial)
p1 <- avg_predictions(mod, type = "invlink(link)")$estimate
p2 <- mod$family$linkinv(mean(predict(mod, type = "link")))
expect_equal(p1, p2, tolerance = 1e-6, ignore_attr = TRUE)

p1 <- avg_predictions(mod, by = "cyl", type = "invlink(link)")
p1 <- p1[order(p1$cyl), "estimate"]
p2 <- tapply(predict(mod, type = "link"), mtcars$cyl, mean)
p2 <- mod$family$linkinv(as.vector(p2))
expect_equal(p1, p2, tolerance = 1e-6, ignore_attr = TRUE)


# Issue #1204: Swapped intervals for models with inverse-link
testthat::skip_if_not_installed("emmeans")
requiet("emmeans")
data(warpbreaks)

mod <- glm(breaks ~ wool * tension, family = Gamma(), data = warpbreaks)
p <- predictions(
    mod,
    newdata = datagrid(grid_type = "balanced"),
    by = c("wool", "tension"),
    type = "invlink(link)"
)
expect_true(all(p$conf.low <= p$conf.high))

mod <- glm(breaks ~ wool * tension, family = Gamma("log"), data = warpbreaks)
p <- predictions(mod, newdata = datagrid(grid_type = "balanced"), by = c("wool", "tension"), type = "invlink(link)")
expect_true(all(p$conf.low <= p$conf.high))
