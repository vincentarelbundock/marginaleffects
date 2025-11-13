# datagrid(x = NA)
# numeric
nd <- datagrid(newdata = mtcars, mpg = NA, hp = 1:4)
expect_equal(nrow(nd), 4, ignore_attr = TRUE)
expect_true(all(is.na(nd$mpg)))

# factor
tmp_typical <- mtcars
tmp_typical$gear <- factor(tmp_typical$gear)
nd <- datagrid(newdata = tmp_typical, gear = NA, hp = 1:4)
expect_equal(nrow(nd), 4, ignore_attr = TRUE)
expect_true(all(is.na(nd$gear)))


# unique values
tmp_typical2 <- mtcars
tmp_typical2$am <- as.logical(tmp_typical2$am)
mod_int <- lm(mpg ~ am * factor(cyl), tmp_typical2)
mfx <- slopes(mod_int, newdata = datagrid(cyl = unique), variables = "am")
expect_equal(nrow(mfx), 3, ignore_attr = TRUE)


# typical FUN_*
tmp_typical3 <- mtcars
tmp_typical3$am <- as.logical(tmp_typical3$am)
tmp_typical3$cyl <- as.factor(tmp_typical3$cyl)
tmp_typical3$gear <- as.character(tmp_typical3$gear)
typ <- datagrid(
    newdata = tmp_typical3,
    FUN_character = max,
    FUN_factor = function(x) sort(x)[1],
    FUN_numeric = stats::median
)
expect_equal(typ$drat, stats::median(mtcars$drat), ignore_attr = TRUE)
expect_equal(typ$cyl, factor("4", levels = c("4", "6", "8")), ignore_attr = TRUE)
expect_equal(typ$gear, "5", ignore_attr = TRUE)


# all manual
mod <- lm(hp ~ mpg, mtcars)
nd <- datagrid(model = mod, mpg = 110)
expect_s3_class(nd, "data.frame")
expect_equal(nrow(nd), 1, ignore_attr = TRUE)


# bugs stay dead: FUN_logical
tmp_typical4 <- mtcars
tmp_typical4$am <- as.logical(tmp_typical4$am)
mod <- lm(mpg ~ am * factor(cyl), data = tmp_typical4)
mfx <- slopes(mod, newdata = datagrid(cyl = unique), variables = "am")
expect_s3_class(mfx, "marginaleffects")
expect_equal(nrow(mfx), 3, ignore_attr = TRUE)

# errors and warning
dat_typical <- mtcars
dat_typical$cyl <- factor(dat_typical$cyl)
dat_typical <- dat_typical
mod <- lm(hp ~ mpg, dat_typical)
expect_error(datagrid(), regexp = "One of")

mod <- lm(hp ~ factor(cyl), dat_typical)
expect_s3_class(datagrid(model = mod, cyl = "4"), "data.frame")
expect_error(datagrid(model = mod, cyl = "2"), regexp = "must be one of the factor levels")
