if (requireNamespace("tinytest", quietly = TRUE) && isTRUE(Sys.getenv("R_NOT_CRAN") == "yes")) {
    tinytest::test_package("marginaleffects")
}
