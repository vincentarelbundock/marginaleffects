if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    isTRUE(Sys.info()["sysname"] != "Windows")) {
    tinytest::test_package("marginaleffects")
}
