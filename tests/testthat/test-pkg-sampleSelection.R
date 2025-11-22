testthat::skip_if_not_installed("sampleSelection")
requiet("sampleSelection")

# Basic expectation tests
data("Mroz87", package = "sampleSelection")
mod_simple <- sampleSelection::heckit(lfp ~ age + faminc, wage ~ exper + educ, data = Mroz87)
expect_slopes2(mod_simple)
expect_predictions2(mod_simple)
expect_hypotheses2(mod_simple)
expect_comparisons2(mod_simple)

dat_sampleSelection <- get_dataset("Mroz87", "sampleSelection")
dat_sampleSelection$kids <- dat_sampleSelection$kids5 + dat_sampleSelection$kids618 > 0
dat_sampleSelection <<- dat_sampleSelection

# heckit: se not supported yet
mod <- heckit(lfp ~ age + I(age^2) + faminc + kids + educ, wage ~ exper + I(exper^2) + educ + city, data = dat_sampleSelection)
mfx <- slopes(mod)
expect_s3_class(mfx, "marginaleffects")
mfx <- slopes(mod, part = "selection", type = "link")
expect_s3_class(mfx, "marginaleffects")
mfx <- slopes(mod, part = "outcome", type = "unconditional")
expect_s3_class(mfx, "marginaleffects")
expect_true(all(is.na(mfx$std.error)))


# selection: no validity
mod <- selection(lfp ~ age + I(age^2) + faminc + kids + educ, wage ~ exper + I(exper^2) + educ + city, data = dat_sampleSelection)
mfx <- slopes(mod)
expect_s3_class(mfx, "marginaleffects")
mfx <- slopes(mod, part = "selection", type = "link")
expect_s3_class(mfx, "marginaleffects")
mfx <- slopes(mod, part = "outcome", type = "unconditional")
expect_s3_class(mfx, "marginaleffects")


# New types
data(Mroz87)
m <- selection(lfp ~ educ + age + kids5 + kids618 + nwifeinc, wage >= 5 ~ educ + exper, data = Mroz87)

avg_slopes(m, part = "selection", type = "response")
avg_slopes(m, part = "outcome", type = "unconditional")
