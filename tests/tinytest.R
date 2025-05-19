if (requireNamespace("tinytest", quietly = TRUE)) {
    home <- grepl("(?i)vince", Sys.info()["nodename"])
    local <- dir.exists("tinytest")
    tinytest::test_package("marginaleffects", at_home = home)
}
