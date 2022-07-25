# WARNING: standard errors are different from nnet::multinom() because stats::vcov gives a very difference matrix.

# why `newdata` used to not be supported
# here the `newdata` does not include the individual or choice variabls at all,
# but we still get a prediction. Impossible to know what order the rows are in,
# if `newdata` is balanced, or what group ids to give. `newdata` could be
# completely malformed and we would still produce results. I could make strong
# assumptions about group id being a multiple of number of rows with some
# modulo hacks, but that's bad practice. Example:
# nd <- TravelMode[, 3:ncol(TravelMode)]
# predict(mod, newdata = head(nd, 12))
source("helpers.R", local = TRUE)
if (ON_CI) exit_file("on ci")
requiet("mlogit")
requiet("nnet")
requiet("AER")
requiet("data.table")
data("TravelMode", package = "AER")

# no validity
mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
cmp <- comparisons(mod)
pre <- predictions(mod)
tid <- tidy(cmp)
expect_inherits(cmp, "comparisons")
expect_inherits(pre, "predictions")
expect_marginaleffects(mod)
expect_true("group" %in% colnames(tid))

# error on bad newdata
mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
nd <- head(TravelMode, 5)
expect_error(comparisons(mod, newdata = nd), pattern = "number of choices")

# mlogit doesn't install on Github actions, so we can't have it in DESCRIPTION,
# but if we use the Fishing data, this raises an error in check()

# vs. nnet::multinom
data("Fishing", package = "mlogit")
dat <<- Fishing
Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
m1 <- mlogit(mode ~ 0 | income, data = Fish)
m2 <- nnet::multinom(mode ~ income, data = Fishing, trace = FALSE)

# predictions() vs. nnet::multinom()
p1 <- predictions(m1)
p2 <- predictions(m2, type = "probs")
setDT(p1, key = c("rowid", "group"))
setDT(p2, key = c("rowid", "group"))
expect_equivalent(p1$predicted, p2$predicted, tolerance = 1e-5)
expect_true(cor(p1$predicted, p2$predicted) > .98)

# comparisons() vs. nnet::multinom()
c1 <- comparisons(m1)
c2 <- comparisons(m2, type = "probs")
setDT(c1, key = c("rowid", "term", "group"))
setDT(c2, key = c("rowid", "term", "group"))
expect_equivalent(c1$comparison, c2$comparison, tolerance = 1e-5)
expect_true(cor(c1$comparison, c2$comparison) > .98)

# marginaleffects() vs. nnet::multinom()
mfx1 <- marginaleffects(m1)
mfx2 <- marginaleffects(m2, type = "probs")
setDT(mfx1, key = c("rowid", "term", "group"))
setDT(mfx2, key = c("rowid", "term", "group"))
expect_equivalent(mfx1$dydx, mfx2$dydx, tolerance = 1e-5)
expect_true(cor(mfx1$dydx, mfx2$dydx) > .98)

