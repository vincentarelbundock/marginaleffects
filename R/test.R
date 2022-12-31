expect_trash <- function(x) {
    out <- tinytest::tinytest(
        result = is.numeric(x),
        call = sys.call(sys.parent(1)),
        diff = "trash")
    out <- tinytest::expect_true(checkmate::check_numeric(x))
    return(out)
}