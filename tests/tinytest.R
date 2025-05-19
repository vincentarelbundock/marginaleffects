NOT_CRAN <- isTRUE(Sys.getenv("R_NOT_CRAN") == "true")
if (NOT_CRAN) tinytest::test_package("marginaleffects")
