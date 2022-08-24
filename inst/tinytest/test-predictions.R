source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("pscl")

tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp)


# bugfix: counterfactual predictions keep rowid
mod <- lm(mpg ~ hp + am, mtcars)
pred <- predictions(mod, newdata = datagrid(am = 0:1, grid.type = "counterfactual"))
expect_predictions(pred, n_row = 64)
expect_true("rowidcf" %in% colnames(pred))


# `variables` argument: character vector
p <- predictions(mod, variables = list("am" = 0:1))
expect_equivalent(nrow(p), 64)

p <- predictions(mod, variables = list("am" = 0:1), newdata = "mean")
expect_equivalent(nrow(p), 2)

# `variables` argument: character vector
expect_error(predictions(mod, variables = list(2)), pattern = "names")
expect_error(predictions(mod, variables = "am"), pattern = "list")


# average prediction with delta method are asymptotically equivalent to back transformed
set.seed(1024)
N <- 1e5
dat <- data.frame(
    y = rbinom(N, 1, prob = .9),
    x = rnorm(N))
mod <- glm(y ~ x, family = binomial, data = dat)
p1 <- tidy(predictions(mod)) # average prediction outside [0,1]
p2 <- tidy(predictions(mod, type = "link"), transform_avg = insight::link_inverse(mod)) # average prediction inside [0,1]
expect_equivalent(p1$estimate, p2$estimate, tolerance = .001)
expect_equivalent(p1$conf.low, p2$conf.low, tolerance = .01)
expect_equivalent(p1$conf.high, p2$conf.high, tolerance = .01)

# average prediction with delta method can produce CI outside [0,1] interval
set.seed(1022)
N <- 10
dat <- data.frame(
    y = rbinom(N, 1, prob = .8),
    x = rnorm(N))
mod <- glm(y ~ x, family = binomial, data = dat)
p1 <- tidy(predictions(mod)) # average prediction outside [0,1]
p2 <- tidy(predictions(mod, type = "link"), transform_avg = insight::link_inverse(mod)) # average prediction outside [0,1]
expect_true(p1$conf.high > 1)
expect_true(p2$conf.high < 1)



################
#  conf.level  #
################

# conf.level argument changes width of interval
mod <- lm(mpg ~ hp + am, mtcars)
for (L in c(.4, .7, .9, .95, .99, .999)) {
    nd <- datagrid(model = mod)
    unknown <- predictions(mod, newdata = nd, conf.level = L)
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
pre2 <- aggregate(predicted ~ am, FUN = mean, data = pre2)
expect_equivalent(pre1$predicted, pre2$predicted)


#########################################
#  weigted average adjusted predictions #
#########################################
pre1 <- predictions(mod, wts = mtcars$w)
pre2 <- predictions(mod)
tid1 <- tidy(pre1)
tid2 <- tidy(pre2)
expect_equivalent(pre1$predicted, pre2$predicted)
expect_true(all(tid1$predicted != tid2$predicted))



######################################
#  values against predict benchmark  #
######################################
mod <- lm(mpg ~ hp + wt + factor(cyl) + am, data = tmp)
nd <- datagrid(model = mod, cyl = c(4, 6, 8))
mm <- predictions(mod, newdata = nd)
expect_equivalent(mm$predicted, unname(predict(mod, newdata = nd)))





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
expect_equivalent(length(unique(mm$predicted)), nrow(mm))


# `typical`: missing logical
mm <- predictions(mod, newdata = datagrid(am = TRUE))
expect_equivalent(nrow(mm), 1)


#########################################################################
#  some models do not return data.frame under `insight::get_predicted`  #
#########################################################################

# hurdle predictions
data("bioChemists", package = "pscl")
mod <- hurdle(art ~ phd + fem | ment, data = bioChemists, dist = "negbin")
pred <- predictions(mod)
expect_inherits(pred, "data.frame")
expect_true("predicted" %in% colnames(pred))





############## DEPRECATED USE OF VARIABLES
# ##############################
# #  size: variables argument  #
# ##############################

# # `variables` arg: factor
# mm <- predictions(mod, variables = "cyl")
# expect_equivalent(nrow(mm), 3)


# # `variables` arg: logical
# mm <- predictions(mod, variables = "am")
# expect_equivalent(nrow(mm), 2)


# # `variables` arg: numeric
# mm <- predictions(mod, variables = "wt")
# expect_equivalent(nrow(mm), 5)


# # `variables` arg: factor + logical
# mm <- predictions(mod, variables = c("am", "cyl"))
# # logical 2; cyl factor 3
# expect_equivalent(nrow(mm), 2 * 3)



# # `variables` arg: logical + numeric
# mm <- predictions(mod, variables = c("am", "wt"))
# # logical 2; numeric 5 numbers
# expect_equivalent(nrow(mm), 2 * 5)


# # `variables` arg: factor + numeric
# mm <- predictions(mod, variables = c("cyl", "wt"))
# # logical 2; numeric 5 numbers
# expect_equivalent(nrow(mm), 3 * 5)


