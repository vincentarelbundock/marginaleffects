test_that("is_binary works with classic input", {
    expect_equal(
        marginaleffects:::is_binary(1:10),
        isTRUE(all(1:10 %in% 0:1))
    )

    expect_equal(
        marginaleffects:::is_binary(0:1),
        isTRUE(all(0:1 %in% 0:1))
    )

    expect_equal(
        marginaleffects:::is_binary(c(0, 0.5, 1)),
        isTRUE(all(c(0, 0.5, 1) %in% 0:1))
    )
})


test_that("is_binary works with single values", {
    expect_equal(
        marginaleffects:::is_binary(1),
        isTRUE(all(1 %in% 0:1))
    )

    expect_equal(
        marginaleffects:::is_binary(2),
        isTRUE(all(2 %in% 0:1))
    )
})


test_that("is_binary works with missings and NULL", {
    expect_equal(
        marginaleffects:::is_binary(c(0, 0.5, NA, 1)),
        isTRUE(all(c(0, 0.5, NA, 1) %in% 0:1))
    )

    expect_equal(
        marginaleffects:::is_binary(NA),
        isTRUE(all(NA %in% 0:1))
    )

    expect_equal(
        marginaleffects:::is_binary(NULL),
        isTRUE(all(NULL %in% 0:1))
    )
})
