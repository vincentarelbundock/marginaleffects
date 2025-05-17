source("helpers.R")
if (!requiet("flexsurv")) exit_file("flexsurv")


mod <- flexsurvreg(formula = Surv(futime, fustat) ~ age + ecog.ps, data = ovarian, dist = "gengamma")
x <- avg_slopes(mod)
expect_inherits(x, "slopes")
x <- predictions(mod)
expect_inherits(x, "predictions")
x <- comparisons(mod)
expect_inherits(x, "comparisons")
