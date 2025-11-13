# classic input

expect_equal(
    marginaleffects:::is_binary(1:10),
    FALSE,
    ignore_attr = TRUE
)

expect_equal(
    marginaleffects:::is_binary(0:1),
    TRUE,
    ignore_attr = TRUE
)

expect_equal(
    marginaleffects:::is_binary(c(0, 0.5, 1)),
    FALSE,
    ignore_attr = TRUE
)

# with single values

expect_equal(
    marginaleffects:::is_binary(1),
    TRUE,
    ignore_attr = TRUE
)

expect_equal(
    marginaleffects:::is_binary(2),
    FALSE,
    ignore_attr = TRUE
)

# with missings / NULL

expect_equal(
    marginaleffects:::is_binary(c(0, 0.5, NA, 1)),
    FALSE,
    ignore_attr = TRUE
)

expect_equal(
    marginaleffects:::is_binary(NA),
    FALSE,
    ignore_attr = TRUE
)

expect_equal(
    marginaleffects:::is_binary(NULL),
    TRUE,
    ignore_attr = TRUE
)
