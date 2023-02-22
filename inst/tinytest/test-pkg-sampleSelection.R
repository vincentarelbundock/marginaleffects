source("helpers.R")
using("marginaleffects")

requiet("sampleSelection")

dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/sampleSelection/Mroz87.csv")
dat$kids <- dat$kids5 + dat$kids618 > 0
dat <<- dat

# heckit: se not supported yet
mod <- heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ,
              wage ~ exper + I( exper^2 ) + educ + city, 
              data = dat)
mfx <- slopes(mod)
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "selection", type = "link")
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "outcome", type = "unconditional")
expect_inherits(mfx, "marginaleffects")
expect_true(all(is.na(mfx$std.error)))


# selection: no validity
mod <- selection(lfp ~ age + I( age^2 ) + faminc + kids + educ,
                 wage ~ exper + I( exper^2 ) + educ + city, 
                 data = dat)
mfx <- slopes(mod)
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "selection", type = "link")
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "outcome", type = "unconditional")
expect_inherits(mfx, "marginaleffects")


source("helpers.R")
rm(list = ls())