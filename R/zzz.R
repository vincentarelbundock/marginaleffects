.onLoad = function(libname, pkgname) {
    if (isTRUE(require("tinytest")) && packageVersion("tinytest") >= "1.4.0") {
        tinytest::register_tinytest_extension(
            "marginaleffects",
            c("expect_marginaleffects", "expect_predictions", "expect_margins", "expect_marginalmeans"))
    }
}