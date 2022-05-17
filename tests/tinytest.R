# skip all tests on CRAN
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    if (requireNamespace("tinytest", quietly = TRUE)) {
        tinytest::test_package("marginaleffects")
    }
}
