source("helpers.R", local = TRUE)

# marginaleffects::mean_or_mode
x <- factor(c("a", "a", "b"))
expect_equivalent(marginaleffects::mean_or_mode(x), x[1])
expect_equivalent(marginaleffects::mean_or_mode(c("a", "a", "b")), "a")
expect_equivalent(marginaleffects::mean_or_mode(1:3), 2)
expect_equivalent(marginaleffects::mean_or_mode(c(FALSE, FALSE, TRUE)), FALSE)
expect_equivalent(marginaleffects::mean_or_mode(data.frame(x = 1:3))$x, 2)

