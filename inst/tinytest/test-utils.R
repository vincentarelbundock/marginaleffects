source("helpers.R")
using("marginaleffects")

# classic input

expect_equivalent(
    marginaleffects:::is_binary(1:10),
    FALSE
)

expect_equivalent(
    marginaleffects:::is_binary(0:1),
    TRUE
)

expect_equivalent(
    marginaleffects:::is_binary(c(0, 0.5, 1)),
    FALSE
)

# with single values

expect_equivalent(
    marginaleffects:::is_binary(1),
    TRUE
)

expect_equivalent(
    marginaleffects:::is_binary(2),
    FALSE
)

# with missings / NULL

expect_equivalent(
    marginaleffects:::is_binary(c(0, 0.5, NA, 1)),
    FALSE
)

expect_equivalent(
    marginaleffects:::is_binary(NA),
    FALSE
)

expect_equivalent(
    marginaleffects:::is_binary(NULL),
    TRUE
)
