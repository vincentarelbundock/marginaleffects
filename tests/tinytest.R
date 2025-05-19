pkg <- requireNamespace("tinytest", quietly = TRUE)
home <- grepl("(?i)vince", Sys.info()["nodename"])
files <- dir.exists("tinytest")
if (pkg && home && files) {
    tinytest::test_package("marginaleffects")
}
