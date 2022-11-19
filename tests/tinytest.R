if (requireNamespace("tinytest", quietly = TRUE) && isTRUE(Sys.getenv("R_NOT_CRAN") == "true")) {
    tinytest::test_package("marginaleffects")
}
