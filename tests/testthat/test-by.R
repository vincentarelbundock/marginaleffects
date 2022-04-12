requiet("margins")
requiet("dplyr")
tol <- 1e-4
tol_se <- 1e-3

test_that("marginaleffects poisson vs. margins", {
    dat <- mtcars
    mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
    mfx <- marginaleffects(
        mod,
        newdata = datagrid(cyl = dat$cyl,
                           am = dat$am,
                           grid.type = "counterfactual"))
    tid <- tidy(mfx, by = c("cyl", "am")) %>% arrange(term, cyl, am)
    mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
    mar <- summary(mar)
    expect_equal(tid$estimate, mar$AME, ignore_attr = TRUE, tolerance = tol)
    expect_equal(tid$std.error, mar$SE, ignore_attr = TRUE, tolerance = tol_se)
})


test_that("comparisons poisson vs. margins", {
    dat <- mtcars
    dat$cyl <- factor(dat$cyl)
    dat$am <- as.logical(dat$am)
    mod <- glm(gear ~ cyl + am, family = poisson, data = dat)
    mfx <- comparisons(
        mod,
        newdata = datagrid(cyl = dat$cyl,
                           am = dat$am,
                           grid.type = "counterfactual"))
    mfx <- tidy(mfx, by = c("cyl", "am")) %>% arrange(term, contrast, cyl, am)
    mar <- margins(mod, at = list(cyl = unique(dat$cyl), am = unique(dat$am)))
    mar <- summary(mar)
    expect_equal(mfx$estimate, mar$AME, ignore_attr = TRUE, tolerance = tol)
    expect_equal(mfx$std.error, mar$SE, ignore_attr = TRUE, tolerance = tol_se)
})


test_that("input checks", {
    mod <- lm(mpg ~ hp, mtcars)
    mfx <- marginaleffects(mod)
    com <- comparisons(mod)
    expect_error(tidy(mfx, by = "am"), regexp = "by` argument")
    expect_error(tidy(com, by = "am"), regexp = "by` argument")
})
