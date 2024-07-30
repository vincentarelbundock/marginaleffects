if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    dir.exists(here::here("inst/tinytest"))) {
    tinytest::test_package("marginaleffects")
}
