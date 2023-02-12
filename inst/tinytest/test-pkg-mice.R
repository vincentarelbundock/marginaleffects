source("helpers.R")
exit_if_not(requiet("mice"))

dat <- iris
dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice <- mice::mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)
mod <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat)

mfx <- suppressWarnings(inferences(avg_slopes(mod, by = "Species"), method = "mi", midata = dat_mice))
expect_inherits(mfx, "comparisons")
expect_equivalent(nrow(mfx), 4)