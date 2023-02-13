source("helpers.R")
exit_if_not(requiet("mice"))

dat <- iris
dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
dat_mice <- mice::mice(dat, m = 20, printFlag = FALSE, .Random.seed = 1024)
mir <- with(dat_mice, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))
mod <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat)

mfx1 <- suppressWarnings(avg_slopes(mir, by = "Species"))
mfx2 <- avg_slopes(mod, by = "Species")
expect_inherits(mfx1, "slopes")
expect_equivalent(nrow(mfx1), nrow(mfx2))