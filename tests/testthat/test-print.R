mod <- lm(mpg ~ hp * factor(gear), mtcars)
expect_snapshot(predictions(mod))
expect_snapshot(predictions(mod, by = "gear"))

## guides()-related error in diffObj. Does not seem marginaleffects-related
# expect_snapshot(comparisons(mod))
expect_snapshot(comparisons(mod, by = "gear"))


# Issue #638: keep datagrid() explicit variables in print
dat_print <- get_dataset("Titanic", "Stat2Data")
m <- glm(Survived ~ Age * PClass * SexCode, data = dat_print, family = binomial)
p <- predictions(m, newdata = datagrid(PClass = unique, SexCode = 0:1))
expect_snapshot(p)

# Issue #1270
mod <- lm(hp ~ mpg * factor(am), mtcars)
cmp <- avg_comparisons(mod, variables = "am", by = "am")
expect_snapshot(cmp)

# twitter Kurz request
mod <- lm(mpg ~ hp * factor(am), mtcars)
expect_snapshot(
    comparisons(mod, variables = "am", newdata = data.frame(am = 0:1, hp = 120))
)

expect_snapshot(
    comparisons(mod, variables = "am", newdata = datagrid(am = 0:1, hp = 120))
)

expect_snapshot(
    predictions(mod, newdata = data.frame(am = 0:1, hp = 120))
)

# Test from tmp.R - predictions with by argument and subsetting
mod_tmp <- lm(mpg ~ hp + am + factor(gear), data = mtcars)
p_tmp <- predictions(mod_tmp, by = c("am", "gear"))
result_tmp <- subset(p_tmp, am == 1)
expect_snapshot(result_tmp)

# Issue #1579: y ~ x | z where z gets printed
mod <- lm(mpg ~ hp, mtcars)
expect_snapshot(
    predictions(mod, hypothesis = ~ I(mean(x)) | cyl)
)
expect_snapshot(
    comparisons(mod, hypothesis = ~ I(mean(x)) | cyl)
)
