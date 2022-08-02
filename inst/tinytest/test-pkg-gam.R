source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("gam")
requiet("emmeans")
requiet("broom")

# gam: marginaleffects vs. emtrends
data(kyphosis, package = "gam")
model <- gam::gam(Kyphosis ~ s(Age,4) + Number, family = binomial, data = kyphosis)
expect_marginaleffects(model)

# emmeans
mfx <- marginaleffects(model, newdata = datagrid(Age = 60, Number = 4), variables = "Number", type = "link")
em <- emtrends(model, ~Number, "Number", at = list(Age = 60, Number = 4))
em <- tidy(em)
expect_equivalent(mfx$dydx, em$Number.trend)
# low tolerance only for CRAN Atlas test
expect_equivalent(mfx$std.error, em$std.error, tolerance = .001)


# gam: predictions: no validity
data(kyphosis, package = "gam")
model <- gam::gam(Kyphosis ~ s(Age, 4) + Number,
            family = binomial, data = kyphosis)
pred1 <- predictions(model)
pred2 <- predictions(model, newdata = head(kyphosis))
expect_predictions(pred1, se = TRUE)
expect_predictions(pred2, n_row = 6, se = TRUE)


# gam: marginalmeans vs. emmeans
data(kyphosis, package = "gam")
tmp <- kyphosis
tmp$categ <- as.factor(sample(letters[1:5], nrow(tmp), replace = TRUE))
model <- gam::gam(Kyphosis ~ s(Age, 4) + Number + categ,
            family = binomial, data = tmp)
mm1 <- marginalmeans(model)
em1 <- data.frame(emmeans(model, specs = "categ", type = "response"))
mm2 <- marginalmeans(model, type = "link")
em2 <- data.frame(emmeans(model, specs = "categ"))

expect_equivalent(mm1$marginalmean, em1$prob)
expect_equivalent(mm2$marginalmean, em2$emmean)

exit_file("gam: marginal means `std.error` does not match `emmeans`")
expect_equivalent(mm1$conf.low, em1$asymp.LCL)
expect_equivalent(mm1$conf.high, em1$asymp.UCL)
expect_equivalent(mm2$conf.low, em1$asymp.LCL)
expect_equivalent(mm2$conf.high, em1$asymp.UCL)
