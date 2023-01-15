source("helpers.R")
using("marginaleffects")

exit_if_not(requiet("emmeans"))


# simple contrasts: no validity check
dat <- mtcars
dat$am <- as.logical(dat$am)
mod <- lm(mpg ~ hp + am + factor(cyl), data = dat)
mfx <- suppressWarnings(slopes(mod))
res <- tidy(mfx)
expect_inherits(res, "data.frame")
expect_equivalent(nrow(res), 4)


# contrast as difference and CI make sense
# problem reported with suggested fix by E.Book in Issue 58
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/palmerpenguins/penguins.csv")
dat$large_penguin <- ifelse(dat$body_mass_g > median(dat$body_mass_g, na.rm = TRUE), 1, 0)
dat <- dat
mod <- glm(large_penguin ~ bill_length_mm + flipper_length_mm + species,
       data = dat, family = binomial)
mfx <- slopes(mod)
ti <- tidy(mfx)
reject_ci <- ti$conf.high < 0 | ti$conf.low > 0
reject_p <- ti$p.value < 0.05
expect_equivalent(reject_ci, reject_p)


# bug be dead: all levels appear
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp <- tmp
mod <- lm(mpg ~ am + factor(cyl), tmp)
mfx = slopes(mod, newdata = datagrid(cyl = c(4, 6)))
expect_equivalent(nrow(mfx), 6)


# numeric contrasts
mod <- lm(mpg ~ hp, data = mtcars)
expect_error(comparisons(mod, contrast_numeric = "bad", variables = "hp"), pattern = "invalid")
contr1 <- comparisons(mod, contrast_numeric = 1, variables = "hp")
contr2 <- comparisons(mod, contrast_numeric = "iqr", variables = "hp")
contr3 <- comparisons(mod, contrast_numeric = "minmax", variables = "hp")
contr4 <- comparisons(mod, contrast_numeric = "sd", variables = "hp")
contr5 <- comparisons(mod, contrast_numeric = "2sd", variables = "hp")
iqr <- diff(stats::quantile(mtcars$hp, probs = c(.25, .75))) * coef(mod)["hp"]
minmax <- (max(mtcars$hp) - min(mtcars$hp)) * coef(mod)["hp"]
sd1 <- sd(mtcars$hp) * coef(mod)["hp"]
sd2 <- 2 * sd(mtcars$hp) * coef(mod)["hp"]
expect_equivalent(contr2$estimate, rep(iqr, 32))
expect_equivalent(contr3$estimate, rep(minmax, 32))
expect_equivalent(contr4$estimate, rep(sd1, 32))
expect_equivalent(contr5$estimate, rep(sd2, 32))


# factor: linear model
mod <- lm(mpg ~ factor(cyl), data = mtcars)
ti <- tidy(comparisons(mod, contrast_factor = "reference"))
re <- coef(mod)[2:3]
expect_equivalent(ti$estimate, re)

ti <- tidy(comparisons(mod, contrast_factor = "pairwise"))
pw <- c(coef(mod)[2:3], coef(mod)[3] - coef(mod)[2])
expect_equivalent(ti$estimate, pw)

ti <- tidy(comparisons(mod, contrast_factor = "sequential"))
se <- c(coef(mod)[2], coef(mod)[3] - coef(mod)[2])
expect_equivalent(ti$estimate, se)


# factor glm
mod <- glm(am ~ factor(cyl), data = mtcars, family = binomial)
pred <- predictions(mod, newdata = datagrid(cyl = mtcars$cyl))
contr <- tidy(comparisons(mod))
expect_equivalent(contr$estimate[1], pred$estimate[pred$cyl == 6] - pred$estimate[pred$cyl == 4])
expect_equivalent(contr$estimate[2], pred$estimate[pred$cyl == 8] - pred$estimate[pred$cyl == 4])


# emmeans w/ back-transforms is similar to comparisons with direct delta method
tol <- 1e-4

dat <- mtcars
dat$cyl <- as.factor(dat$cyl)
dat <- dat
mod <- glm(am ~ cyl, data = dat, family = binomial)

# link scale
cmp <- comparisons(mod, type = "link", newdata = datagrid(), contrast_factor = "pairwise")
emm <- emmeans(mod, specs = "cyl")
emm <- emmeans::contrast(emm, method = "revpairwise", df = Inf, adjust = NULL)
emm <- data.frame(confint(emm))
expect_equivalent(cmp$estimate, emm$estimate)
expect_equivalent(cmp$std.error, emm$SE)
expect_equivalent(cmp$conf.low, emm$asymp.LCL)
expect_equivalent(cmp$conf.high, emm$asymp.UCL)

# response scale
cmp <- comparisons(mod, type = "response", newdata = datagrid(), contrast_factor = "pairwise")
emm <- emmeans(mod, specs = "cyl")
emm <- emmeans::contrast(regrid(emm), method = "revpairwise", df = Inf, adjust = NULL,
type = "response", ratios = FALSE)
emm <- data.frame(confint(emm))
expect_equivalent(cmp$estimate, emm$estimate, tolerance = tol)
expect_equivalent(cmp$std.error, emm$SE, tolerance = tol)
expect_equivalent(cmp$conf.low, emm$asymp.LCL, tolerance = tol)
expect_equivalent(cmp$conf.high, emm$asymp.UCL, tolerance = tol)



# smart contrast labels
dat <- mtcars
dat$am <- as.logical(dat$am)
dat$cyl <- as.factor(dat$cyl)
dat$gear <- as.character(dat$gear)
dat <- dat
mod <- lm(mpg ~ hp + am + cyl + gear, data = dat)

cmp1 <- comparisons(
    mod,
    newdata = "mean")
expect_equivalent(
    cmp1$contrast,
    c("+1", "TRUE - FALSE", "6 - 4", "8 - 4", "4 - 3", "5 - 3"))

cmp2 <- comparisons(
    mod,
    contrast_numeric = "sd",
    transform_pre = "ratio",
    newdata = "mean")
expect_equivalent(
    cmp2$contrast,
    c("(x + sd/2) / (x - sd/2)", "TRUE / FALSE", "6 / 4", "8 / 4", "4 / 3", "5 / 3"))

cmp3 <- comparisons(
    mod,
    contrast_numeric = "iqr",
    transform_pre = "lnratioavg",
    newdata = "mean")
expect_equivalent(
    cmp3$contrast,
    c("ln(mean(Q3) / mean(Q1))", "ln(mean(TRUE) / mean(FALSE))", "ln(mean(6) / mean(4))", "ln(mean(8) / mean(4))", "ln(mean(4) / mean(3))", "ln(mean(5) / mean(3))"))

