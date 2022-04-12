# No longer used. Kept here in case we want to support `numDeriv` in the future

get_gradient <- function(func, x) {

    # global options are used to switch between homemade and numDeriv
    eps <- getOption("marginaleffects_deriv_eps", default = 0.0001)
    marginaleffects_numDeriv <- getOption("marginaleffects_numDeriv", default = NULL)
    flag <- is.null(marginaleffects_numDeriv)

    # dependency-free
    if (isTRUE(flag)) {
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
