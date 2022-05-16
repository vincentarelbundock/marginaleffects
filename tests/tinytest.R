source("helpers.R")
requiet("tinytest")
requiet("marginaleffects")

if (requireNamespace("tinytest", quietly = TRUE)) {
    tinytest::test_package("marginaleffects")
}

