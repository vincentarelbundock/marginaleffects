NOT_CRAN <- isTRUE(Sys.getenv("R_NOT_CRAN") == "true")
TINYTEST <- requireNamespace("tinytest", quietly = TRUE)

if (TINYTEST && NOT_CRAN) {
    tinytest::test_package("marginaleffects")
}
