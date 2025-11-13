# Issue #769
m = glm(am ~ mpg + hp, data = mtcars, family = binomial)
get_slopes_at_value = function(m, x) {
    slopes(m, newdata = datagrid(mpg = x))
}
a = get_slopes_at_value(m, .5)
b = slopes(m, newdata = datagrid(mpg = .5))
expect_equal(a, b, ignore_attr = TRUE)
