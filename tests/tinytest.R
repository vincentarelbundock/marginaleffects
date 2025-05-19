run <- FALSE
pkg <- requireNamespace("tinytest", quietly = TRUE)
home <- grepl("(?i)vince", Sys.info()["nodename"]) || NOT_CRAN
if (pkg && home && run) {
    tinytest::test_package("marginaleffects")
}
