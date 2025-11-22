testthat::skip_if(!EXPENSIVE, "EXPENSIVE")

testthat::skip_if_not_installed("MASS")
testthat::skip_if_not_installed("ordinal")
requiet("MASS")
requiet("ordinal")

# Basic expectation tests
data("wine", package = "ordinal")
mod_simple <- ordinal::clm(rating ~ temp + contact, data = wine)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

dat_ordinal <- get_dataset("housing", "MASS")
for (i in seq_along(dat_ordinal)) {
    if (is.character(dat_ordinal[[i]])) {
        dat_ordinal[[i]] <- factor(dat_ordinal[[i]])
    }
}

# marginaleffects: clm: vs. MASS
known <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = dat_ordinal, Hess = TRUE)

known <- suppressMessages(avg_slopes(known, type = "probs"))
unknown <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = dat_ordinal)
unknown <- avg_slopes(unknown)
expect_equal(unknown$estimate, known$estimate, tolerance = .00001, ignore_attr = TRUE)
expect_equal(unknown$std.error, known$std.error, tolerance = .00001, ignore_attr = TRUE)


# marginaleffects: protect against corner cases
# do not convert numeric to factor in formula
stata <- readRDS(testing_path("stata/stata.rds"))[["MASS_polr_01"]]
dat_ordinal2 <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
mod <- ordinal::clm(factor(y) ~ x1 + x2, data = dat_ordinal2)
expect_error(slopes(mod), regexp = "Please convert the variable to factor")


# marginaleffects: clm: vs. Stata
stata <- readRDS(testing_path("stata/stata.rds"))[["MASS_polr_01"]]
dat_ordinal3 <- read.csv(testing_path("stata/databases/MASS_polr_01.csv"))
dat_ordinal3$y <- factor(dat_ordinal3$y)
dat_ordinal3 <- dat_ordinal3
mod <- ordinal::clm(y ~ x1 + x2, data = dat_ordinal3)
mfx <- avg_slopes(mod)
mfx <- merge(mfx, stata)
expect_equal(mfx$estimate, mfx$dydxstata, tolerance = .001, ignore_attr = TRUE)
expect_equal(mfx$std.error, mfx$std.errorstata, tolerance = .001, ignore_attr = TRUE)
expect_slopes2(mod)


# Issue 717: no validity
data("wine", package = "ordinal")
mod <- clm(rating ~ contact + temp, data = wine)
p <- predictions(mod, type = "linear.predictor")
expect_s3_class(p, "predictions")
p <- predictions(mod, type = "cum.prob")
expect_s3_class(p, "predictions")
expect_error(predictions(mod, type = "junk"), regexp = "Assertion")
p <- avg_slopes(mod, type = "cum.prob")
expect_s3_class(p, "slopes")


# marginaleffects: clm: no validity
tmp_ordinal <- get_dataset("soup", "ordinal")
tab26 <- with(tmp_ordinal, table("Product" = PROD, "Response" = SURENESS))
dimnames(tab26)[[2]] <- c("Sure", "Not Sure", "Guess", "Guess", "Not Sure", "Sure")
dat26 <- expand.grid(sureness = as.factor(1:6), prod = c("Ref", "Test"))
dat26$wghts <- c(t(tab26))
dat26 <- dat26
m1 <- clm(sureness ~ prod, scale = ~prod, data = dat26, weights = wghts, link = "logit")
m2 <- update(m1, link = "probit")
m3 <- update(m1, link = "cloglog")
m4 <- update(m1, link = "loglog")
m5 <- update(m1, link = "cauchit", start = coef(m1))
expect_slopes2(m1, n_unique = 6)
expect_slopes2(m2, n_unique = 6)
expect_slopes2(m3, n_unique = 6)
expect_slopes2(m4, n_unique = 6)
expect_slopes2(m5, n_unique = 6)


# Issue #718: incorrect standard errors when scale and location are the same
dat_ordinal4 <- transform(mtcars, cyl = factor(cyl), vs2 = vs)
mod1 <- clm(
    cyl ~ hp + vs, # vs has a location effect
    scale = ~vs, # vs also has a scale effect
    data = dat_ordinal4
)
mod2 <- clm(
    cyl ~ hp + vs, # vs has a location effect
    scale = ~vs2, # vs also has a scale effect
    data = dat_ordinal4
)
nd <- subset(dat_ordinal4, select = -cyl)
pre1 <- predictions(mod1)
pre2 <- predictions(mod2)
pre3 <- predict(mod1, newdata = nd, type = "prob", se.fit = TRUE)
expect_equal(pre1$estimate, pre2$estimate, ignore_attr = TRUE)
expect_equal(pre1$std.error, pre2$std.error, ignore_attr = TRUE)
expect_equal(subset(pre1, group == 4)$estimate, pre3$fit[, 1], ignore_attr = TRUE)
expect_equal(subset(pre1, group == 4)$std.error, pre3$se.fit[, 1], tolerance = 1e-4, ignore_attr = TRUE)


# Issue #718: incorrect
dat_ordinal5 <- transform(mtcars, cyl = factor(cyl))
mod <- suppressWarnings(clm(cyl ~ vs + carb, scale = ~vs, nominal = ~carb, data = dat_ordinal5))
dat_ordinal5$cyl <- NULL
p1 <- predictions(mod)
p2 <- suppressWarnings(predict(mod, newdata = dat_ordinal5, se.fit = TRUE))
expect_equal(subset(p1, group == 4)$estimate, p2$fit[, 1], ignore_attr = TRUE)
expect_equal(subset(p1, group == 4)$std.error, p2$se.fit[, 1], tolerance = 1e4, ignore_attr = TRUE)
expect_equal(subset(p1, group == 6)$estimate, p2$fit[, 2], ignore_attr = TRUE)
expect_equal(subset(p1, group == 6)$std.error, p2$se.fit[, 2], tolerance = 1e4, ignore_attr = TRUE)
expect_equal(subset(p1, group == 8)$estimate, p2$fit[, 3], ignore_attr = TRUE)
expect_equal(subset(p1, group == 8)$std.error, p2$se.fit[, 3], tolerance = 1e4, ignore_attr = TRUE)


# Issue #729
dat_ordinal6 <- transform(
    mtcars,
    cyl = factor(
        cyl,
        levels = c(4, 6, 8),
        labels = c("small", "medium", "large")
    )
)
mod <- clm(cyl ~ hp + carb, scale = ~vs, data = dat_ordinal6)
mfx <- avg_slopes(mod, slope = "eyex")
expect_s3_class(mfx, "slopes")
mfx <- avg_slopes(mod, slope = "dyex")
expect_s3_class(mfx, "slopes")

p <- predictions(mod)
mfx1 <- slopes(mod, variables = "carb", slope = "dydx")
mfx2 <- slopes(mod, variables = "carb", slope = "eyex")
mfx3 <- slopes(mod, variables = "carb", slope = "eydx")
mfx4 <- slopes(mod, variables = "carb", slope = "dyex")
expect_equal(mfx2$estimate, mfx1$estimate * (mfx1$carb / p$estimate), ignore_attr = TRUE)
expect_equal(mfx3$estimate, mfx1$estimate / p$estimate, ignore_attr = TRUE)
expect_equal(mfx4$estimate, mfx1$estimate * mfx1$carb, ignore_attr = TRUE)

mfx1 <- slopes(mod, variables = "hp", slope = "dydx")
mfx2 <- slopes(mod, variables = "hp", slope = "eyex")
mfx3 <- slopes(mod, variables = "hp", slope = "eydx")
mfx4 <- slopes(mod, variables = "hp", slope = "dyex")
expect_equal(mfx2$estimate, mfx1$estimate * (mfx1$hp / p$estimate), ignore_attr = TRUE)
expect_equal(mfx3$estimate, mfx1$estimate / p$estimate, ignore_attr = TRUE)
expect_equal(mfx4$estimate, mfx1$estimate * mfx1$hp, ignore_attr = TRUE)
