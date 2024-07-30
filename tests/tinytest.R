if (!(dir.exists("inst/tinytest") || dir.exists("../inst/tinytest"))) stop(getwd())
if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    (dir.exists("inst/tinytest") || dir.exists("../inst/tinytest"))) {
    tinytest::test_package("marginaleffects")
}
