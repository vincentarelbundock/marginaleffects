source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")
requiet("margins")
tol <- 1e-4
tol_se <- 1e-3

mod <- glm(gear ~ cyl + am, family = poisson, data = mtcars)

# use transform_pre to collapse into averages
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
expect_error(comparisons(mod, transform_pre = "dydxavg"), pattern = "supported")
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
