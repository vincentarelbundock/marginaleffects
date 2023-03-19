source("helpers.R")
using("marginaleffects")
if (!EXPENSIVE) exit_file("EXPENSIVE")
if (packageVersion("base") >= "4.2.0") exit_file("dev breaks this")

requiet("MASS")
requiet("ordinal")

dat <- read.csv(
    "https://vincentarelbundock.github.io/Rdatasets/csv/MASS/housing.csv",
    stringsAsFactors = TRUE)

# marginaleffects: clm: vs. MASS
known <- MASS::polr(Sat ~ Infl + Type + Cont, weights = Freq, data = dat, Hess = TRUE)

known <- tidy(suppressMessages(slopes(known, type = "probs")))
unknown <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = dat)
unknown <- tidy(slopes(unknown))
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
mfx <- slopes(mod)
mfx <- tidy(mfx)
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
tmp <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/ordinal/soup.csv")
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
if (!packageVersion("insight") <= "0.19.0.9") exit_file("insight version")

dat <- transform(mtcars, cyl = factor(cyl), vs2 = vs)
mod1 <- clm(cyl ~ hp + vs,  # vs has a location effect
  scale = ~ vs,    # vs also has a scale effect
  data = dat)
mod2 <- clm(cyl ~ hp + vs,  # vs has a location effect
  scale = ~ vs2,    # vs also has a scale effect
  data = dat)
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
mod <- suppressWarnings(clm(cyl ~ vs + carb, scale = ~ vs, nominal = ~ carb, data = dat))
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
dat <- transform(mtcars,
    cyl = factor(
        cyl,
        levels = c(4, 6, 8),
        labels = c("small", "medium", "large")))
mod <- clm(cyl ~ hp + carb, scale = ~vs, data = dat)
mfx <- avg_slopes(mod, slope = "eyex")
expect_inherits(mfx, "slopes")
mfx <- avg_slopes(mod, slope = "dyex")
expect_inherits(mfx, "slopes")

mfx1 <- slopes(mod, variables = "carb", slope = "dydx")
mfx2 <- slopes(mod, variables = "carb", slope = "eydx")
mfx3 <- slopes(mod, variables = "carb", slope = "dyex")
mfx4 <- slopes(mod, variables = "carb", slope = "eyex")
expect_equivalent(mfx2$estimate, mfx1$estimate / mfx1$predicted)
expect_equivalent(mfx3$estimate, mfx1$estimate * mfx1$carb)
expect_equivalent(mfx4$estimate, mfx1$estimate / mfx1$predicted * mfx1$carb)

mfx1 <- slopes(mod, slope = "dydx") |> subset(term == "hp")
mfx2 <- slopes(mod, slope = "eydx") |> subset(term == "hp")
mfx3 <- slopes(mod, slope = "dyex") |> subset(term == "hp")
mfx4 <- slopes(mod, slope = "eyex") |> subset(term == "hp")
expect_equivalent(mfx2$estimate, mfx1$estimate / mfx1$predicted)
expect_equivalent(mfx3$estimate, mfx1$estimate * mfx1$hp)
expect_equivalent(mfx4$estimate, mfx1$estimate / mfx1$predicted * mfx1$hp)




source("helpers.R")
rm(list = ls())