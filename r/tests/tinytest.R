run <- TRUE
pkg <- requireNamespace("tinytest", quietly = TRUE)
home <- grepl("(?i)vince", Sys.info()["nodename"]) || identical(Sys.getenv("R_NOT_CRAN"), "true")
testdir <- dir.exists("inst/tinytest")
if (pkg && home && run && testdir) {
    tinytest::test_package("marginaleffects")
}
