source("helpers.R")
using("marginaleffects")


# datagrid(x = NA)
# numeric
nd <- datagrid(newdata = mtcars, mpg = NA, hp = 1:4)
expect_equivalent(nrow(nd), 4)
expect_true(all(is.na(nd$mpg)))

# factor
tmp <- mtcars
tmp$gear <- factor(tmp$gear)
nd <- datagrid(newdata = tmp, gear = NA, hp = 1:4)
expect_equivalent(nrow(nd), 4)
expect_true(all(is.na(nd$gear)))


# unique values
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod_int <- lm(mpg ~ am * factor(cyl), tmp)
mfx <- slopes(mod_int,
                       newdata = datagrid(cyl = unique),
                       variables = "am")
expect_equivalent(nrow(mfx), 3)


# typical FUN_*
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp$cyl <- as.factor(tmp$cyl)
tmp$gear <- as.character(tmp$gear)
typ <- datagrid(
    newdata = tmp,
    FUN_character = max,
    FUN_factor = function(x) sort(x)[1],
    FUN_numeric = stats::median)
expect_equivalent(typ$drat, stats::median(mtcars$drat))
expect_equivalent(typ$cyl, factor("4", levels = c("4", "6", "8")))
expect_equivalent(typ$gear, "5")


# all manual
mod <- lm(hp ~ mpg, mtcars)
nd <- datagrid(model = mod, mpg = 110)
expect_inherits(nd, "data.frame")
expect_equivalent(nrow(nd), 1)


# bugs stay dead: FUN_logical
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am * factor(cyl), data = tmp)
mfx <- slopes(mod, newdata = datagrid(cyl = unique), variables = "am")
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 3)

# errors and warnings
exit_file("works interactively")
dat <- mtcars
dat$cyl <- factor(dat$cyl)
dat <- dat
mod <- lm(hp ~ mpg, dat)
expect_error(datagrid(), pattern = "are both .NULL")

mod <- lm(hp ~ factor(cyl), dat)
expect_inherits(datagrid(model = mod, cyl = "4"), "data.frame")
expect_error(datagrid(model = mod, cyl = "2"), pattern = "must be one of the factor levels")


rm(list = ls())