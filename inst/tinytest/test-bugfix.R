source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")

# Bug stay dead: Issue 55
# Error: Argument 1 must have names.
# vab: possibly caused by a version of `emmeans` < 1.6.3
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, 
       data = dat, family = binomial)
mfx <- marginaleffects(mod, variables = "species")
expect_inherits(mfx, "data.frame")
expect_true(nrow(mfx) > 0)
expect_true(ncol(mfx) > 0)


# Hernan & Robins replication: bug would not detect `as.factor()` in formula()
nhefs <- read.csv("https://raw.githubusercontent.com/vincentarelbundock/modelarchive/main/data-raw/nhefs.csv")
f <- wt82_71 ~ qsmk + sex + race + age + I(age*age) + factor(education) +
     smokeintensity + I(smokeintensity*smokeintensity) + smokeyrs +
     I(smokeyrs*smokeyrs) + as.factor(exercise) + as.factor(active) + wt71 +
     I(wt71*wt71) + I(qsmk*smokeintensity)

fit <- glm(f, data = nhefs)
pre <- predictions(fit, newdata = nhefs)
cmp <- comparisons(fit, newdata = nhefs)
mfx <- marginaleffects(fit, newdata = nhefs)
expect_inherits(pre, "predictions")
expect_inherits(cmp, "comparisons")
expect_inherits(mfx, "marginaleffects")



# Issue 372: reserved variable names
dat <- mtcars
dat$group <- dat$am
mod <- lm(mpg ~ group, data = dat)
expect_warning(comparisons(mod), pattern = "forbidden")
expect_error(suppressWarnings(comparisons(mod)), pattern = "change")
mod <- lm(mpg ~ group + hp, data = dat)
expect_warning(comparisons(mod), pattern = "forbidden")
expect_equivalent(nrow(suppressWarnings(comparisons(mod))), 32)
