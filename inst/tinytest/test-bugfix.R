source("helpers.R")
using("marginaleffects")


# Bug stay dead: Issue 55
# Error: Argument 1 must have names.
# vab: possibly caused by a version of `emmeans` < 1.6.3
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species, 
       data = dat, family = binomial)
mfx <- slopes(mod, variables = "species")
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
mfx <- slopes(fit, newdata = nhefs)
cmp <- comparisons(fit, newdata = nhefs)
expect_inherits(pre, "predictions")
expect_inherits(cmp, "comparisons")
expect_inherits(mfx, "marginaleffects")



# Issue 372: reserved variable names
dat <- mtcars
dat$group <- dat$am
mod <- lm(mpg ~ group, data = dat)
expect_error(comparisons(mod), pattern = "forbidden")
mod <- lm(mpg ~ group + hp, data = dat)
expect_error(comparisons(mod), pattern = "forbidden")


exit_file("works interactively")

# Issue #556
set.seed(12345)
n = 500
x = sample(1:3, n, replace = TRUE)
y = rnorm(n)
z = ifelse(x + y + rlogis(n) > 1.5, 1, 0)
dat = data.frame(x = factor(x), y = y, z = z)
dat <- dat

m1 = glm(z ~ x + y, family = binomial, data = dat)
nd <- datagrid(model = m1, y = seq(-2.5, 2.5, by = 0.25))
p1 <- predictions(m1, newdata = nd, type = "link")
p2 <- as.data.frame(predict(m1, newdata = nd, se.fit = TRUE))

expect_equal(p1$estimate, p2$fit)
expect_equal(p1$std.error, p2$se.fit)

set.seed(12345)
n = 60
x = sample(1:3, n, replace = TRUE)
z = ifelse(x + rlogis(n) > 1.5, 1, 0)
dat = data.frame(x = factor(x), z = z)
dat <- dat

m2 = glm(z ~ I(x==2) + I(x==3), family = binomial, data = dat)

p1 <- predictions(m2, type = "link")
p2 <- predictions(m2, newdata = dat, type = "link")
p3 <- as.data.frame(predict(m2, se.fit = TRUE, type = "link"))

expect_equal(p1$estimate, p3$fit)
expect_equal(p1$std.error, p3$se.fit)
expect_equal(p2$estimate, p3$fit)
expect_equal(p2$std.error, p3$se.fit)


# Issue #671
dta <- data.frame(
     lab = sample(0:1, size = 1000, replace = T),
     age_group = sample(c("old", "young"), size = 1000, replace = TRUE))
mod <- lm(lab ~ age_group, dta)
mfx <- avg_slopes(mod)
expect_equivalent(nrow(mfx), 1)
expect_true("young - old" %in% mfx$contrast)


# Issue #697
dat <- data.frame(
  outcome = rbinom(n = 100, size = 1, prob = 0.35),
  var_binom = as.factor(rbinom(n = 100, size = 1, prob = 0.2)),
  var_cont = rnorm(n = 100, mean = 10, sd = 7),
  group = sample(letters[1:4], size = 100, replace = TRUE),
  groups = sample(letters[1:4], size = 100, replace = TRUE))

m1 <- glm(
    outcome ~ var_binom + var_cont + group,
    data = dat, family = binomial())
expect_error(avg_slopes(m1), pattern = "forbidden")
expect_error(avg_slopes(m1, variables = "var_cont"), pattern = "forbidden")

m2 <- glm(
    outcome ~ var_binom + var_cont + groups,
    data = dat, family = binomial())
expect_inherits(avg_slopes(m2), "slopes")
expect_inherits(avg_slopes(m2, variables = "var_cont"), "slopes")


# Issue #723
dat <- data.frame(
  rbind(
    c(10., 'A', 'AU'),
    c(20., 'A', 'AU'),
    c(30., 'A', 'AU'),
    c(20., 'B', 'AU'),
    c(30., 'B', 'AU'),
    c(40., 'B', 'AU'),
    c(10., 'B', 'NZ'),
    c(20., 'B', 'NZ'),
    c(30., 'B', 'NZ'),
    c(20., 'A', 'NZ'),
    c(30., 'A', 'NZ'),
    c(40., 'A', 'NZ')
  )
)
colnames(dat) <- c('y', 'treatment', 'country')
mod <- lm(y ~ treatment * country, dat)
cmp <- comparisons(mod, variables = 'treatment', by = 'country')
expect_inherits(cmp, "comparisons")
expect_equivalent(nrow(cmp), 2)
expect_equivalent(
  cmp$estimate[cmp$country == "AU"],
  coef(mod)["treatmentB"])
expect_equivalent(
  cmp$estimate[cmp$country == "NZ"],
  coef(mod)["treatmentB"] + coef(mod)["treatmentB:countryNZ"])




source("helpers.R")
rm(list = ls())

