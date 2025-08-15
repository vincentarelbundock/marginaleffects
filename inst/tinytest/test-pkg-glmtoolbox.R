source("helpers.R")
using("marginaleffects")
requiet("glmtoolbox")

data(spruces)
mod <- size ~ poly(days, 4) + treat
fit <- glmgee(mod, id = tree, family = Gamma(log), corstr = "AR-M-dependent(1)", data = spruces)

s <- avg_slopes(fit)
expect_inherits(s, "slopes")
p <- avg_predictions(fit, by = "treat")
expect_inherits(p, "predictions")
k <- avg_comparisons(fit)
expect_inherits(k, "comparisons")
