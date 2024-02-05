if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    isTRUE(Sys.info()["sysname"] != "Windows") &&
    # do not run test when checking package because .Rbuildignore excludes the modelarchive directory
    dir.exists("inst/tinytest/modelarchive")) {
    tinytest::test_package("marginaleffects")
}
