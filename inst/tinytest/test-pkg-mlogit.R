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
source("helpers.R")
using("marginaleffects")
if (ON_CI) exit_file("on ci")
requiet("nnet")
requiet("mlogit")
requiet("data.table")

TravelMode <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/TravelMode.csv")
TravelMode$X <- NULL # {mlogit} assumes first column is the index
mod <- mlogit(choice ~ wait + gcost | income + size, data = TravelMode)

# no validity
mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
cmp <- comparisons(mod)
pre <- predictions(mod)
tid <- tidy(cmp)
expect_inherits(cmp, "comparisons")
expect_inherits(pre, "predictions")
expect_slopes(mod)
expect_true("group" %in% colnames(tid))

# error on bad newdata
mod <- mlogit(choice ~ wait + gcost | income + size, TravelMode)
nd <- head(TravelMode, 5)
expect_error(comparisons(mod, newdata = nd), pattern = "number of choices")

# mlogit doesn't install on Github actions, so we can't have it in DESCRIPTION,
# but if we use the Fishing data, this raises an error in check()

# vs. nnet::multinom
data("Fishing", package = "mlogit")
dat <- Fishing
Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
m1 <- mlogit(mode ~ 0 | income, data = Fish)
m2 <- nnet::multinom(mode ~ income, data = Fishing, trace = FALSE)

# predictions() vs. nnet::multinom()
p1 <- predictions(m1)
p2 <- predictions(m2, type = "probs")
setDT(p1, key = c("rowid", "group"))
setDT(p2, key = c("rowid", "group"))
expect_equivalent(p1$estimate, p2$estimate, tolerance = 1e-5)
expect_true(cor(p1$estimate, p2$estimate) > .98)

# comparisons() vs. nnet::multinom()
c1 <- comparisons(m1)
c2 <- comparisons(m2, type = "probs")
setDT(c1, key = c("rowid", "term", "group"))
setDT(c2, key = c("rowid", "term", "group"))
expect_equivalent(c1$estimate, c2$estimate, tolerance = 1e-5)
expect_true(cor(c1$estimate, c2$estimate) > .98)

# slopes() vs. nnet::multinom()
mfx1 <- slopes(m1)
mfx2 <- slopes(m2, type = "probs")
setDT(mfx1, key = c("rowid", "term", "group"))
setDT(mfx2, key = c("rowid", "term", "group"))
expect_equivalent(mfx1$estimate, mfx2$estimate, tolerance = 1e-5)
expect_true(cor(mfx1$estimate, mfx2$estimate) > .98)

# Issue #551
mod1 <- mlogit(choice ~ wait + gcost | income + size, TravelMode) 
mfx <- slopes(mod1, variables = c("income", "size"))
expect_inherits(mfx, "marginaleffects")

TravelMode$dsize <- ifelse(TravelMode$size == "1", 1, 0)
mod2 <- mlogit(choice ~ wait + gcost | income + dsize, TravelMode) 
mfx <- slopes(mod2, variables = c("income", "dsize"))
expect_inherits(mfx, "marginaleffects")

TravelMode$dsize <- as.factor(TravelMode$dsize)
mod3 <- mlogit(choice ~ wait + gcost | income + dsize, TravelMode) 
mfx <- slopes(mod3, variables = c("income", "dsize"))
expect_inherits(mfx, "marginaleffects")



rm(list = ls())