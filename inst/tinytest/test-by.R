source("helpers.R")
requiet("margins")
tol <- 1e-4
tol_se <- 1e-3

# marginaleffects poisson vs. margins
dat <- mtcars
mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
mfx <- marginaleffects(
mod,
newdata = datagrid(cyl = dat$cyl,
                   am = dat$am,
                   grid.type = "counterfactual"))
tid <- tidy(mfx, by = c("cyl", "am"))
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
newdata = datagrid(cyl = dat$cyl,
                   am = dat$am,
                   grid.type = "counterfactual"))
mfx <- tidy(mfx, by = c("cyl", "am"))
mfx <- mfx[order(mfx$term, mfx$contrast, mfx$cyl, mfx$am),]
mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
mar <- summary(mar)
expect_equivalent(mfx$estimate, mar$AME, tolerance = tol)
expect_equivalent(mfx$std.error, mar$SE, tolerance = tol_se)


# input checks
mod <- lm(mpg ~ hp, mtcars)
mfx <- marginaleffects(mod)
com <- comparisons(mod)
expect_error(tidy(mfx, by = "am"), pattern = "by` argument")
expect_error(tidy(com, by = "am"), pattern = "by` argument")
