source("helpers.R")
using("marginaleffects")
requiet("survival")
requiet("emmeans")
requiet("broom")

# clogit 
N  <- 10000
ng <- 5000
exd <- data.frame(
    g = rep(1:ng, each = N / ng),
    out = rep(0L:1L, N / 2),
    x = sample(0L:1L, N / 2, prob = c(.8, .2), replace = TRUE))
mod <- clogit(
    out ~ x + strata(g),
    method = "exact",
    data = exd)

mfx <- slopes(mod, type = "lp")
expect_inherits(mfx, "marginaleffects")
cmp <- comparisons(mod, type = "lp")
expect_inherits(cmp, "comparisons")
pre <- predictions(mod, type = "lp")
expect_inherits(pre, "predictions")


# coxph vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))$survival_coxph_01
test1 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                     status = c(1, 1, 1, 0, 1, 1, 0),
                     x = c(0, 2, 1, 1, 1, 0, 0),
                     sex = factor(c(0, 0, 0, 0, 1, 1, 1)))
mod <- coxph(Surv(time, status) ~ x + strata(sex),
             data = test1,
             ties = "breslow")
mfx <- merge(avg_slopes(mod, type = "lp"), stata)
expect_slopes(mod, type = "risk", n_unique = 4)
expect_equivalent(mfx$estimate, mfx$dydxstata)
expect_equivalent(mfx$std.error, mfx$std.errorstata)

# emtrends
em <- emtrends(mod, ~x, "x", at = list(time = 4, status = 1, x = 0, sex = factor(0, levels = 0:1)))
em <- tidy(em)
mfx <- slopes(mod, variables = "x", type = "lp")
expect_equivalent(mfx$estimate[1], em$x.trend)
expect_equivalent(mfx$std.error[1], em$std.error)



# coxph: no validity
test2 <<- data.frame(start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
                     stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
                     event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
                     x = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0))
mod <- coxph(Surv(start, stop, event) ~ x, test2)
expect_slopes(mod, type = "risk", n_unique = 2)



# bugs stay dead: conf.level forces get_predicted which doesn't process 'type'
test3 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                     status = c(1, 1, 1, 0, 1, 1, 0),
                     x = c(0, 2, 1, 1, 1, 0, 0),
                     sex = factor(c(0, 0, 0, 0, 1, 1, 1)))
mod <- coxph(Surv(time, status) ~ x + strata(sex),
             data = test3,
             ties = "breslow")
p1 <- predictions(mod, type = "lp")
p2 <- predictions(mod, type = "risk")
expect_true(all(p1$estimate != p2$estimate))



# bugs stay dead: numeric vs factor strata
#skip_if_not_installed("insight", minimum_version = "0.17.0")
stata <- readRDS(testing_path("stata/stata.rds"))$survival_coxph_01
test4 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                     status = c(1, 1, 1, 0, 1, 1, 0),
                     x = c(0, 2, 1, 1, 1, 0, 0),
                     sex = factor(c(0, 0, 0, 0, 1, 1, 1)))
test5 <<- data.frame(time = c(4, 3, 1, 1, 2, 2, 3),
                     status = c(1, 1, 1, 0, 1, 1, 0),
                     x = c(0, 2, 1, 1, 1, 0, 0),
                     sex = c(0, 0, 0, 0, 1, 1, 1))
mod1 <- coxph(Surv(time, status) ~ x + strata(sex),
              data = test4,
              ties = "breslow")
mod2 <- coxph(Surv(time, status) ~ x + strata(sex),
              data = test5,
              ties = "breslow")

mfx1 <- merge(avg_slopes(mod1, type = "lp"), stata)
mfx2 <- merge(avg_slopes(mod2, type = "lp"), stata)
expect_equivalent(mfx1$estimate, mfx2$estimate)





source("helpers.R")
suppressWarnings(rm(list = paste0("test", 1:5), .GlobalEnv))
rm(list = ls())