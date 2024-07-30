if (!isTRUE(Sys.getenv("R_NOT_CRAN") == "true")) stop("R_NOT_CRAN")
if (!requireNamespace("tinytest", quietly = TRUE)) stop("tinytest package")
if (!dir.exists("inst/tinytest")) stop("directory")
if (requireNamespace("tinytest", quietly = TRUE) &&
    isTRUE(Sys.getenv("R_NOT_CRAN") == "true") &&
    dir.exists("inst/tinytest")) {
    tinytest::test_package("marginaleffects")
}
