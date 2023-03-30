source("helpers.R")
requiet("phylolm")

set.seed(123456)
tre = rcoal(60)
taxa = sort(tre$tip.label)
b0 = 0
b1 = 1
x <- rTrait(
    n = 1, phy = tre, model = "BM",
    parameters = list(ancestral.state = 0, sigma2 = 10))
y <- b0 + b1 * x +
    rTrait(n = 1, phy = tre, model = "lambda", parameters = list(
        ancestral.state = 0, sigma2 = 1, lambda = 0.5))
dat <<- data.frame(trait = y[taxa], pred = x[taxa])
fit <- phylolm(trait ~ pred, data = dat, phy = tre, model = "lambda")
pre <- predictions(fit)
cmp <- comparisons(fit)
mfx <- avg_slopes(fit)
expect_inherits(pre, "predictions")
expect_inherits(cmp, "comparisons")
expect_inherits(mfx, "slopes")


source("helpers.R")
rm(list = ls())