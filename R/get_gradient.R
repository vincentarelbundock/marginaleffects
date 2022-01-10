# adapted from the numDeriv package for R by Paul Gilbert published under GPL2 license

get_gradient <- function(func, x) {

    # global options are used to switch between homemade and numDeriv
    marginaleffects_numDeriv <- getOption("marginaleffects_numDeriv", default = NULL)
    flag <- is.null(marginaleffects_numDeriv)

    # dependency-free
    if (isTRUE(flag)) {
        eps <- 1e-4
        df <- (func(x + eps) - func(x)) / eps

    # numDeriv package (more flexible)
    } else {
        assert_dependency("numDeriv")
        checkmate::assert_list(marginaleffects_numDeriv)
        f <- get("grad", asNamespace("numDeriv"))
        marginaleffects_numDeriv[["func"]] <- func
        marginaleffects_numDeriv[["x"]] <- x
        df <- do.call("f", marginaleffects_numDeriv)
    }

    return(df)
}
