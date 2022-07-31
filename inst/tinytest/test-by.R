source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("margins")
tol <- 1e-4
tol_se <- 1e-3

mod1 <- glm(gear ~ cyl + am, family = poisson, data = mtcars)
mod2 <- lm(gear ~ cyl + am, data = mtcars)
p1 <- predictions(mod1, by = "am")
p2 <- predictions(mod2, by = "am")
p3 <- predictions(mod2, by = "am", wts = mtcars$wt)
expect_false("conf.low" %in% colnames(p1))
expect_true("conf.low" %in% colnames(p2))
expect_equivalent(nrow(p1), nrow(p2))
expect_equivalent(nrow(p1), 2)


# use transform_pre to collapse into averages
mod <- glm(gear ~ cyl + am, family = poisson, data = mtcars)
x <- tidy(comparisons(mod, transform_pre = "dydx"))
y <- comparisons(mod, transform_pre = "dydxavg")
expect_equivalent(x$estimate, y$comparison)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, transform_pre = "eyex"))
y <- comparisons(mod, transform_pre = "eyexavg")
expect_equivalent(x$estimate, y$comparison)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, transform_pre = "eydx"))
y <- comparisons(mod, transform_pre = "eydxavg")
expect_equivalent(x$estimate, y$comparison)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(comparisons(mod, transform_pre = "dyex"))
y <- comparisons(mod, transform_pre = "dyexavg")
expect_equivalent(x$estimate, y$comparison)
expect_equivalent(x$std.error, y$std.error)

x <- tidy(marginaleffects(mod, slope = "dyex"))
y <- marginaleffects(mod, slope = "dyexavg")
expect_equivalent(x$estimate, y$dydx)
expect_equivalent(x$std.error, y$std.error)

# input sanity check
expect_error(marginaleffects(mod, slope = "bad"), pattern = "eyexavg")

# by is deprecated in `summary()` and `tidy()`
expect_error(summary(comparisons(mod), by = "am"), pattern = "instead")
expect_error(tidy(comparisons(mod), by = "am"), pattern = "instead")

# by argument
mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)
cmp <- comparisons(mod, by = "am", transform_pre = "lnor")
expect_equal(nrow(cmp), 4)

cmp <- comparisons(mod, by = "am")
tid <- tidy(cmp)
expect_equivalent(nrow(tid), nrow(cmp))
expect_equivalent(nrow(tid), 4)
expect_true("am" %in% colnames(tid))

# not supported in bayesian models
mod <- insight::download_model("brms_1")
expect_error(comparisons(mod, by = "am"), pattern = "supported")


# marginaleffects poisson vs. margins
dat <- mtcars
mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
mfx <- marginaleffects(
    mod,
    by = c("cyl", "am"),
    newdata = datagrid(
        cyl = dat$cyl,
        am = dat$am,
        grid.type = "counterfactual"))
tid <- tidy(mfx)
tid <- tid[order(tid$term, tid$cyl, tid$am),]
mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
mar <- summary(mar)
expect_equivalent(tid$estimate, mar$AME, tolerance = tol)
expect_equivalent(tid$std.error, mar$SE, tolerance = tol_se)


# comparisons poisson vs. margins
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat$am <- as.logical(dat$am)
mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
mfx <- comparisons(
    mod,
    by = c("cyl", "am"),
    newdata = datagrid(
        cyl = dat$cyl,
        am = dat$am,
        grid.type = "counterfactual"))
mfx <- tidy(mfx)
mfx <- mfx[order(mfx$term, mfx$contrast, mfx$cyl, mfx$am),]
mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
mar <- summary(mar)
expect_equivalent(mfx$estimate, mar$AME, tolerance = tol)
expect_equivalent(mfx$std.error, mar$SE, tolerance = tol_se)


# input checks
mod <- lm(mpg ~ hp, mtcars)
expect_error(comparisons(mod, by = "am", pattern = "newdata"))
expect_error(marginaleffects(mod, by = "am"), pattern = "newdata")


# counterfactual margins at()
dat <- mtcars
dat$cyl <- factor(dat$cyl)
mod <- lm(mpg ~ factor(cyl) * hp + wt, data = dat)
mar <- margins(mod, at = list(cyl = unique(dat$cyl)))
mar <- data.frame(summary(mar))
mfx <- marginaleffects(
    mod,
    by = "cyl",
    newdata = datagridcf(cyl = c(4, 6, 8)))
expect_equivalent(mfx$dydx, mar$AME)
expect_equivalent(mfx$std.error, mar$SE, tolerance = 1e6)



# issue #434 by with character precitors
dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/AER/Affairs.csv")
mod <- glm(
    affairs ~ children + gender + yearsmarried,
    family = poisson,
    data = dat)
p <- predictions(mod, by = "children")
expect_equivalent(nrow(p), 2)
expect_false(anyNA(p$predicted))

