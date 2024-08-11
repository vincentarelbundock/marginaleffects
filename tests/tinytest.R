if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    !any(grepl("tinytest", readLines(here::here(".Rbuildignore"))))) {
    tinytest::test_package("marginaleffects")
}
