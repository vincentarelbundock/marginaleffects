source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("sampleSelection")

data("Mroz87", package = "sampleSelection")
Mroz87$kids  <- (Mroz87$kids5 + Mroz87$kids618 > 0)
dat <<- Mroz87

# heckit: se not supported yet
mod <- heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ,
              wage ~ exper + I( exper^2 ) + educ + city, 
              data = dat)
mfx <- marginaleffects(mod)
expect_inherits(mfx, "marginaleffects")
mfx <- marginaleffects(mod, part = "selection", type = "link")
expect_inherits(mfx, "marginaleffects")
mfx <- marginaleffects(mod, part = "outcome", type = "unconditional")
expect_inherits(mfx, "marginaleffects")
expect_true(all(is.na(mfx$std.error)))


# selection: no validity
mod <- selection(lfp ~ age + I( age^2 ) + faminc + kids + educ,
                 wage ~ exper + I( exper^2 ) + educ + city, 
                 data = dat)
mfx <- marginaleffects(mod)
expect_inherits(mfx, "marginaleffects")
mfx <- marginaleffects(mod, part = "selection", type = "link")
expect_inherits(mfx, "marginaleffects")
mfx <- marginaleffects(mod, part = "outcome", type = "unconditional")
expect_inherits(mfx, "marginaleffects")

