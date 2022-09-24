source("helpers.R", local = TRUE)
if (ON_CRAN) exit_file("on cran")

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
mfx <- marginaleffects(mod_int,
                       newdata = datagrid(cyl = tmp$cyl),
                       variables = "am")
expect_equivalent(nrow(mfx), 3)


# typical FUN.*
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
tmp$cyl <- as.factor(tmp$cyl)
tmp$gear <- as.character(tmp$gear)
typ <- datagrid(
    newdata = tmp,
    FUN.character = max,
    FUN.factor = function(x) sort(x)[1],
    FUN.numeric = stats::median)
expect_equivalent(typ$drat, stats::median(mtcars$drat))
expect_equivalent(typ$cyl, factor("4", levels = c("4", "6", "8")))
expect_equivalent(typ$gear, "5")


# all manual
mod <- lm(hp ~ mpg, mtcars)
nd <- datagrid(model = mod, mpg = 110)
expect_inherits(nd, "data.frame")
expect_equivalent(dim(nd), c(1, 1))


# errors and warnings
mod <- lm(hp ~ mpg, mtcars)
expect_error(datagrid(), pattern = "are both .NULL")

mod <- lm(hp ~ factor(cyl), mtcars)
expect_inherits(datagrid(model = mod, cyl = "4"), "data.frame")
expect_error(datagrid(model = mod, cyl = "2"), pattern = "must be one of the factor levels")


# bugs stay dead: FUN.logical
tmp <- mtcars
tmp$am <- as.logical(tmp$am)
mod <- lm(mpg ~ am * factor(cyl), data = tmp)
mfx <- marginaleffects(mod, newdata = datagrid(cyl = tmp$cyl), variables = "am")
expect_inherits(mfx, "marginaleffects")
expect_equivalent(nrow(mfx), 3)

