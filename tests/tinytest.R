if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    dir.exists("inst/tinytest")) {
    stop("intentional breakage")
    tinytest::test_package("marginaleffects")
}
