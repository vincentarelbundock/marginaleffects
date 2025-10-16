source("helpers.R")
using("marginaleffects")
if (!EXPENSIVE) exit_file("EXPENSIVE")

requiet("MASS")
requiet("ordinal")

# Basic expectation tests
data("wine", package = "ordinal")
mod_simple <- ordinal::clm(rating ~ temp + contact, data = wine)
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

dat <- get_dataset("housing", "MASS")
for (i in seq_along(dat)) {
    if (is.character(dat[[i]])) {
        dat[[i]] <- factor(dat[[i]])
    }
}

# marginaleffects: clm: vs. MASS
known <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = dat, Hess = TRUE)

known <- suppressMessages(avg_slopes(known, type = "probs"))
unknown <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = dat)
unknown <- avg_slopes(unknown)
expect_equivalent(unknown$estimate, known$estimate, tolerance = .00001)
expect_equivalent(unknown$std.error, known$std.error, tolerance = .00001)


# marginaleffects: protect against corner cases
# do not convert numeric to factor in formula
stata <- readRDS(testing_path("stata/stata.rds"))[["MASS_polr_01"]]
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
mod <- ordinal::clm(factor(y) ~ x1 + x2, data = dat)
expect_error(slopes(mod), pattern = "Please convert the variable to factor")


# marginaleffects: clm: vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))[["MASS_polr_01"]]
dat <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
dat$y <- factor(dat$y)
dat <- dat
mod <- ordinal::clm(y ~ x1 + x2, data = dat)
mfx <- avg_slopes(mod)
mfx <- merge(mfx, stata)
expect_equivalent(mfx$estimate, mfx$dydxstata, tolerance = .001)
expect_equivalent(mfx$std.error, mfx$std.errorstata, tolerance = .001)
expect_slopes(mod)


# Issue 717: no validity
data("wine", package = "ordinal")
mod <- clm(rating ~ contact + temp, data = wine)
p <- predictions(mod, type = "linear.predictor")
expect_inherits(p, "predictions")
p <- predictions(mod, type = "cum.prob")
expect_inherits(p, "predictions")
expect_error(predictions(mod, type = "junk"), pattern = "Assertion")
p <- avg_slopes(mod, type = "cum.prob")
expect_inherits(p, "slopes")


# marginaleffects: clm: no validity
tmp <- get_dataset("soup", "ordinal")
tab26 <- with(tmp, table("Product" = PROD, "Response" = SURENESS))
dimnames(tab26)[[2]] <- c("Sure", "Not Sure", "Guess", "Guess", "Not Sure", "Sure")
dat26 <- expand.grid(sureness = as.factor(1:6), prod = c("Ref", "Test"))
dat26$wghts <- c(t(tab26))
dat26 <- dat26
m1 <- clm(sureness ~ prod, scale = ~prod, data = dat26, weights = wghts, link = "logit")
m2 <- update(m1, link = "probit")
m3 <- update(m1, link = "cloglog")
m4 <- update(m1, link = "loglog")
m5 <- update(m1, link = "cauchit", start = coef(m1))
expect_slopes(m1, n_unique = 6)
expect_slopes(m2, n_unique = 6)
expect_slopes(m3, n_unique = 6)
expect_slopes(m4, n_unique = 6)
expect_slopes(m5, n_unique = 6)


# Issue #718: incorrect standard errors when scale and location are the same
dat <- transform(mtcars, cyl = factor(cyl), vs2 = vs)
mod1 <- clm(
    cyl ~ hp + vs, # vs has a location effect
    scale = ~vs, # vs also has a scale effect
    data = dat
)
mod2 <- clm(
    cyl ~ hp + vs, # vs has a location effect
    scale = ~vs2, # vs also has a scale effect
    data = dat
)
nd <- subset(dat, select = -cyl)
pre1 <- predictions(mod1)
pre2 <- predictions(mod2)
pre3 <- predict(mod1, newdata = nd, type = "prob", se.fit = TRUE)
expect_equivalent(pre1$estimate, pre2$estimate)
expect_equivalent(pre1$std.error, pre2$std.error)
expect_equivalent(subset(pre1, group == 4)$estimate, pre3$fit[, 1])
expect_equivalent(subset(pre1, group == 4)$std.error, pre3$se.fit[, 1], tol = 1e-4)


# Issue #718: incorrect
dat <- transform(mtcars, cyl = factor(cyl))
mod <- suppressWarnings(clm(cyl ~ vs + carb, scale = ~vs, nominal = ~carb, data = dat))
dat$cyl <- NULL
p1 <- predictions(mod)
p2 <- suppressWarnings(predict(mod, newdata = dat, se.fit = TRUE))
expect_equivalent(subset(p1, group == 4)$estimate, p2$fit[, 1])
expect_equivalent(subset(p1, group == 4)$std.error, p2$se.fit[, 1], tol = 1e4)
expect_equivalent(subset(p1, group == 6)$estimate, p2$fit[, 2])
expect_equivalent(subset(p1, group == 6)$std.error, p2$se.fit[, 2], tol = 1e4)
expect_equivalent(subset(p1, group == 8)$estimate, p2$fit[, 3])
expect_equivalent(subset(p1, group == 8)$std.error, p2$se.fit[, 3], tol = 1e4)


# Issue #729
dat <- transform(
    mtcars,
    cyl = factor(
        cyl,
        levels = c(4, 6, 8),
        labels = c("small", "medium", "large")
    )
)
mod <- clm(cyl ~ hp + carb, scale = ~vs, data = dat)
mfx <- avg_slopes(mod, slope = "eyex")
expect_inherits(mfx, "slopes")
mfx <- avg_slopes(mod, slope = "dyex")
expect_inherits(mfx, "slopes")

p <- predictions(mod)
mfx1 <- slopes(mod, variables = "carb", slope = "dydx")
mfx2 <- slopes(mod, variables = "carb", slope = "eyex")
mfx3 <- slopes(mod, variables = "carb", slope = "eydx")
mfx4 <- slopes(mod, variables = "carb", slope = "dyex")
expect_equivalent(mfx2$estimate, mfx1$estimate * (mfx1$carb / p$estimate))
expect_equivalent(mfx3$estimate, mfx1$estimate / p$estimate)
expect_equivalent(mfx4$estimate, mfx1$estimate * mfx1$carb)

mfx1 <- slopes(mod, variables = "hp", slope = "dydx")
mfx2 <- slopes(mod, variables = "hp", slope = "eyex")
mfx3 <- slopes(mod, variables = "hp", slope = "eydx")
mfx4 <- slopes(mod, variables = "hp", slope = "dyex")
expect_equivalent(mfx2$estimate, mfx1$estimate * (mfx1$hp / p$estimate))
expect_equivalent(mfx3$estimate, mfx1$estimate / p$estimate)
expect_equivalent(mfx4$estimate, mfx1$estimate * mfx1$hp)


# clmm2: Basic tests with binomial mixed model
requiet("lme4")
cbpp2 <- rbind(lme4::cbpp[,-(2:3)], lme4::cbpp[,-(2:3)])
cbpp2 <- within(cbpp2, {
    incidence <- as.factor(rep(0:1, each = nrow(lme4::cbpp)))
    freq <- with(lme4::cbpp, c(incidence, size - incidence))
})
mod_clmm2 <- clmm2(incidence ~ period, random = herd, weights = freq, data = cbpp2, Hess = 1)

# Test basic functionality
expect_predictions(mod_clmm2)
expect_comparisons(mod_clmm2)
expect_slopes(mod_clmm2)

# Test that predictions return reasonable values
pred <- predictions(mod_clmm2)
expect_true(all(pred$estimate >= 0 & pred$estimate <= 1))
expect_true(all(!is.na(pred$estimate)))
expect_true(all(!is.na(pred$std.error)))

# Test avg_predictions
avg_pred <- avg_predictions(mod_clmm2)
expect_inherits(avg_pred, "predictions")

# Test avg_comparisons
avg_comp <- avg_comparisons(mod_clmm2)
expect_inherits(avg_comp, "comparisons")


# clmm2: Soup data with symmetric threshold (tests set_coef fix)
dat_soup <- get_dataset("soup", "ordinal")
dat_soup <- subset(dat_soup, as.numeric(dat_soup$RESP) <= 24)
# Ensure variables are factors
dat_soup$SURENESS <- factor(dat_soup$SURENESS, ordered = TRUE)
dat_soup$RESP <- factor(dat_soup$RESP)
dat_soup$RESP <- dat_soup$RESP[drop = TRUE]  # Drop unused levels
dat_soup$PROD <- factor(dat_soup$PROD)  # Ensure PROD is a factor
mod_soup <- clmm2(SURENESS ~ PROD, random = RESP, data = dat_soup,
                  link = "probit", Hess = TRUE, method = "ucminf",
                  threshold = "symmetric")

# Test that set_coef properly updates Theta from Alpha
expect_predictions(mod_soup)
expect_comparisons(mod_soup)
expect_slopes(mod_soup)

# Test predictions
pred_soup <- predictions(mod_soup)
expect_true(all(pred_soup$estimate >= 0 & pred_soup$estimate <= 1))
expect_true(all(!is.na(pred_soup$estimate)))
expect_true(all(!is.na(pred_soup$std.error)))

# Test avg_predictions by group
avg_pred_soup <- avg_predictions(mod_soup, by = "PROD")
expect_inherits(avg_pred_soup, "predictions")
expect_true(nrow(avg_pred_soup) == 2)  # Two levels of PROD

# Test avg_slopes
avg_slopes_soup <- avg_slopes(mod_soup)
expect_inherits(avg_slopes_soup, "slopes")
expect_true(nrow(avg_slopes_soup) == 1)  # One term (PROD)

# Test hypotheses
hyp_soup <- hypotheses(mod_soup)
expect_inherits(hyp_soup, "hypotheses")
expect_true(nrow(hyp_soup) == 4)  # 3 threshold params + 1 beta

# Test different quadrature methods (with Hess=TRUE for vcov)
mod_soup_agq <- update(mod_soup, Hess = TRUE, nAGQ = 3)
pred_agq <- predictions(mod_soup_agq)
expect_inherits(pred_agq, "predictions")
expect_true(all(!is.na(pred_agq$estimate)))
