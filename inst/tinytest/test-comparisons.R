source("helpers.R")
using("marginaleffects")

# examples from the main documentation
mod <- lm(mpg ~ hp, data = mtcars)
cmp <- comparisons(mod, variables = list(hp = c(90, 110)))
expect_inherits(cmp, "comparisons")


# Issue #527
dat <- mtcars
dat$new_hp <- 49 * (dat$hp - min(dat$hp)) / (max(dat$hp) - min(dat$hp)) + 1
dat <- dat
mod <- lm(mpg ~ log(new_hp) + factor(cyl), data = dat)
fdiff <- function(x) data.frame(x, x + 10)
cmp1 <- comparisons(mod, variables = list(new_hp = fdiff))
cmp2 <- comparisons(mod, variables = list(new_hp = 10))
expect_equivalent(nrow(cmp1), 32)
expect_equivalent(nrow(cmp2), 32)


# Issue #720
mod <- lm(mpg ~ hp * qsec, dat = mtcars)
cmp <- avg_comparisons(mod, variables = list(hp = "2sd"))
expect_equivalent(cmp$contrast, "(x + sd) - (x - sd)")


# Issue #622 cross-contrasts
mod <- lm(mpg ~ am * factor(cyl), data = mtcars)
cmp <- comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
expect_equivalent(nrow(cmp), 64)
cmp <- avg_comparisons(mod, variables = c("cyl", "am"), cross = TRUE)
expect_equivalent(nrow(cmp), 2)


# Issue #794
mod <- glm(am ~ hp, data = mtcars, family = binomial())
cmp1 <- comparisons(mod, comparison = "lift")
cmp2 <- comparisons(mod, comparison = "liftavg")
expect_equal(nrow(cmp1), 32)
expect_equal(nrow(cmp2), 1)
expect_error(comparisons(mod, comparison = "liftr"))


# Issue #1669: unique lift labels to avoid collapsing `contrast` col with by
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat$am <- factor(dat$am)
mod <- lm(mpg ~ cyl * am, data = dat)
cmp <- avg_comparisons(
    mod,
    variables = list(cyl = "reference"),
    by = "am",
    type = "response",
    comparison = "lift"
)
expect_equal(nrow(cmp), 4)
expect_equal(length(unique(cmp$contrast)), 2)


# Issue #1120: avg comparison by default with avg_
mod <- glm(vs ~ am + wt, data = mtcars, family = binomial)
d0 <- transform(mtcars, am = 0)
d1 <- transform(mtcars, am = 1)
p0 <- predict(mod, newdata = d0, type = "response")
p1 <- predict(mod, newdata = d1, type = "response")
c1 <- mean(p1 / p0) # marginaleffects 0.20.0
c2 <- mean(p1) / mean(p0) # after bug fix
cmp <- avg_comparisons(mod, variables = "am", comparison = "ratio")
expect_equivalent(cmp$estimate, c2)


# Issue #1137
fit <- lm(mpg ~ vs + am + vs:am, data = mtcars) # equivalent to ~ vs*am
cmp <- avg_comparisons(fit, variables = list(am = c(0, 1), vs = c(1, 0)), cross = TRUE)
expect_equivalent(cmp$estimate, -0.992857142857154)
expect_error(avg_comparisons(fit, variables = list(am = 0, vs = 1:0), cross = TRUE), "length 2")
expect_error(avg_comparisons(fit, variables = list(am = 1:3, vs = 1:0), cross = TRUE), "0 or 1")


# Issue #1151: dedup weights with lift and custom comparison
requiet("tibble")
requiet("dplyr")
set.seed(0)
d <- tibble::tribble(
    ~device, ~lang, ~pop_weight, ~exp_weight, ~p_control, ~p_treat,
    "Desktop", "English", 0.4, 0.8, 0.1, 0.15,
    "Desktop", "Non-English", 0.2, 0.1, 0.04, 0.02,
    "Mobile", "English", 0.3, 0.05, 0.08, 0.03,
    "Mobile", "Non-English", 0.1, 0.05, 0.01, 0.01
)
experiment_data <- d %>%
    sample_n(20000, weight = exp_weight, replace = T) %>%
    group_by(device, lang) %>%
    mutate(
        treatment = sample(c("treatment", "control"), size = n(), replace = T),
        p = case_match(treatment, "treatment" ~ p_treat, .default = p_control),
        y = rbinom(n(), 1, p)
    ) %>%
    ungroup()

model <- glm(y ~ treatment * device * lang, data = experiment_data, family = binomial())
lift <- function(hi, lo) mean(hi - lo) / mean(lo)
c1 <- avg_comparisons(
    model,
    variables = "treatment",
    comparison = "lift"
)
c2 <- avg_comparisons(
    model,
    variables = "treatment",
    comparison = lift
)
d_lo <- transform(experiment_data, treatment = "control")
d_hi <- transform(experiment_data, treatment = "treatment")
p_lo <- predict(model, newdata = d_lo, type = "response")
p_hi <- predict(model, newdata = d_hi, type = "response")
liftavg <- function(hi, lo) (mean(hi - lo)) / mean(lo)
c3 <- liftavg(p_hi, p_lo)

expect_equivalent(c1$estimate, c2$estimate)
expect_equivalent(c1$estimate, c3)
