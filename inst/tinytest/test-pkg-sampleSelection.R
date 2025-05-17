source("helpers.R")
using("marginaleffects")

requiet("sampleSelection")

dat <- get_dataset("Mroz87", "sampleSelection")
dat$kids <- dat$kids5 + dat$kids618 > 0
dat <<- dat

# heckit: se not supported yet
mod <- heckit(lfp ~ age + I(age^2) + faminc + kids + educ, wage ~ exper + I(exper^2) + educ + city, data = dat)
mfx <- slopes(mod)
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "selection", type = "link")
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "outcome", type = "unconditional")
expect_inherits(mfx, "marginaleffects")
expect_true(all(is.na(mfx$std.error)))


# selection: no validity
mod <- selection(lfp ~ age + I(age^2) + faminc + kids + educ, wage ~ exper + I(exper^2) + educ + city, data = dat)
mfx <- slopes(mod)
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "selection", type = "link")
expect_inherits(mfx, "marginaleffects")
mfx <- slopes(mod, part = "outcome", type = "unconditional")
expect_inherits(mfx, "marginaleffects")


# New types
data(Mroz87)
m <- selection(lfp ~ educ + age + kids5 + kids618 + nwifeinc, wage >= 5 ~ educ + exper, data = Mroz87)

avg_slopes(m, part = "selection", type = "response")
avg_slopes(m, part = "outcome", type = "unconditional")


source("helpers.R")
