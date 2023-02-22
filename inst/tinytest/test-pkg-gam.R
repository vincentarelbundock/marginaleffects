source("helpers.R")
using("marginaleffects")

requiet("gam")
requiet("emmeans")
requiet("broom")

# gam: marginaleffects vs. emtrends
data(kyphosis, package = "gam")
model <- gam::gam(Kyphosis ~ gam::s(Age,4) + Number, family = binomial, data = kyphosis)
expect_slopes(model)

# emmeans
mfx <- slopes(model, newdata = datagrid(Age = 60, Number = 4), variables = "Number", type = "link")
em <- emtrends(model, ~Number, "Number", at = list(Age = 60, Number = 4))
em <- tidy(em)
expect_equivalent(mfx$estimate, em$Number.trend)
# low tolerance only for CRAN Atlas test
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)


# gam: predictions: no validity
data(kyphosis, package = "gam")
model <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number,
            family = binomial, data = kyphosis)
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(kyphosis))
expect_predictions(pred1, se = FALSE)
expect_predictions(pred2, n_row = 6, se = FALSE)


# gam: marginalmeans vs. emmeans
# TODO: not clear what happens to smooth
data(kyphosis, package = "gam")
tmp <- kyphosis
tmp$categ <- as.factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
model <- gam::gam(Kyphosis ~ gam::s(Age, 4) + Number + categ, family = binomial, data = tmp)

# `datagrid()` is smarter than `emmeans()` about integers
atlist <- list(Age = round(mean(tmp$Age)), Number = round(mean(tmp$Number)))
mm1 <- marginal_means(model)
em1 <- data.frame(emmeans(model, specs = "categ", type = "response", at = atlist))
mm2 <- marginal_means(model, type = "link")
em2 <- data.frame(emmeans(model, specs = "categ", at = atlist))

expect_equivalent(mm1$estimate, em1$prob)
expect_equivalent(mm2$estimate, em2$emmean)
expect_equivalent(mm1$conf.low, em1$asymp.LCL)
expect_equivalent(mm1$conf.high, em1$asymp.UCL)
expect_equivalent(mm2$conf.low, em2$asymp.LCL)
expect_equivalent(mm2$conf.high, em2$asymp.UCL)



rm(list = ls())