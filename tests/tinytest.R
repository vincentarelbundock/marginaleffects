NOT_CRAN <- isTRUE(Sys.getenv("R_NOT_CRAN") == "true")
TINYTEST <- requireNamespace("tinytest", quietly = TRUE)
LOCAL_TESTS <- dir.exists("inst/tinytest")

if (TINYTEST && NOT_CRAN && LOCAL_TESTS) {
    tinytest::test_package("marginaleffects")
}
