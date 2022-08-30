source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("modelbased")
requiet("emmeans")


# visualisation_matrix() without `x` variable
mod <- lm(mpg ~ hp + factor(cyl), mtcars)

p1 <- predictions(mod, newdata = datagrid(cyl = mtcars$cyl))
p2 <- predictions(mod, newdata = visualisation_matrix(at = "cyl"))
expect_equivalent(nrow(p1), nrow(p2))
expect_true(all(c("newdata_adjusted_for", "newdata_at_specs") %in% names(attributes(p2))))

m1 <- marginaleffects(mod, newdata = datagrid(cyl = mtcars$cyl))
m2 <- marginaleffects(mod, newdata = visualisation_matrix(at = "cyl"))
expect_equivalent(nrow(m1), nrow(m2))
expect_true(all(c("newdata_adjusted_for", "newdata_at_specs") %in% names(attributes(m2))))


# shortcut labels
mod <- glm(vs ~ hp + factor(cyl), family = binomial, data = mtcars)
cmp1 <- comparisons(mod, newdata = "mean")
cmp2 <- comparisons(mod, newdata = "median")
expect_true(all(cmp1$hp == mean(mtcars$hp)))
expect_true(all(cmp2$hp == stats::median(mtcars$hp)))
expect_true(all(cmp2$comparison != cmp1$comparison))



# newdata = 'marginalmeans'
dat <- mtcars
dat$gear <- factor(dat$gear)
dat$cyl <- factor(dat$cyl)
dat$am <- factor(dat$am)
mod <- lm(mpg ~ gear + cyl + am, data = dat)
cmp <- comparisons(mod, newdata = "marginalmeans", variables = "gear")
cmp <- tidy(cmp)

emm <- emmeans(mod, specs = "gear")
emm <- data.frame(contrast(emm, method = "trt.vs.ctrl1"))

expect_equivalent(cmp$estimate, emm$estimate)
expect_equivalent(cmp$std.error, emm$SE)



# interaction: newdata = 'marginalmeans'
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
mod <- lm(bill_length_mm ~ species * sex + island + body_mass_g, data = dat)

cmp <- comparisons(
mod,
newdata = "marginalmeans",
variables = c("species", "island"))

emm <- emmeans(mod, specs = c("species", "island"))
emm <- data.frame(contrast(emm, method = "trt.vs.ctrl1"))

# hack: not sure if they are well aligned
expect_equivalent(sort(cmp$comparison), sort(emm$estimate))
expect_equivalent(sort(cmp$std.error), sort(emm$SE))

