source("helpers.R")
using("marginaleffects")
requiet("rstpm2")

## warning: initial values uses coxph, which warns "the model contains interactions"
suppressWarnings(test1 <- stpm2(Surv(rectime, censrec == 1) ~ hormon * x3, data = brcancer, df = 3))
nd <- data.frame(rectime = 1000, hormon = c(0, 1), x3 = 50)

pred1 <- predict(test1, type = "surv", newdata = nd)
pred2 <- predictions(test1, type = "surv", newdata = nd)
expect_equivalent(pred1, pred2$estimate)

pred3 <- predict(test1, newdata = nd, type = "meansurv")
pred4 <- avg_predictions(test1, type = "surv", newdata = nd, by = "x3")
expect_equivalent(pred3[1], pred4$estimate)

source("helpers.R")
