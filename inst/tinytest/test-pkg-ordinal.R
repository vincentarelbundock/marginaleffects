source("helpers.R")
using("marginaleffects")
exit_if_not(EXPENSIVE)
exit_if_not(!ON_CI)
exit_if_not(packageVersion("base") >= "4.2.0")

exit_if_not(requiet("MASS"))
exit_if_not(requiet("ordinal"))

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



source("helpers.R")
rm(list = ls())