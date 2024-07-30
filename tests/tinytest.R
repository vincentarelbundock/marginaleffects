warning(isTRUE(Sys.getenv("R_NOT_CRAN") == "true"))
warning(requireNamespace("tinytest", quietly = TRUE))
warning(dir.exists("inst/tinytest"))
if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    dir.exists("inst/tinytest")) {
    tinytest::test_package("marginaleffects")
}
