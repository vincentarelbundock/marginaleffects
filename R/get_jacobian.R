# adapted from the numDeriv package for R by Paul Gilbert published under GPL2 license

get_jacobian <- function(func, x) {

    # global options are used to switch between homemade and numDeriv
    marginaleffects_numDeriv <- getOption("marginaleffects_numDeriv", default = NULL)
    flag <- is.null(marginaleffects_numDeriv)

    # dependency-free
    if (isTRUE(flag)) {
        eps <- 1e-4
        baseline <- func(x)
        df <- matrix(NA, length(baseline), length(x))
        for (i in seq_along(x)) {
            dx <- x
            dx[i] <- dx[i] + eps
            df[, i] <- (func(dx) - baseline) / eps
        }

    # numDeriv package (more flexible)
    } else {
        assert_dependency("numDeriv")
        checkmate::assert_list(marginaleffects_numDeriv)
        f <- get("jacobian", asNamespace("numDeriv"))
        marginaleffects_numDeriv[["func"]] <- func
        marginaleffects_numDeriv[["x"]] <- x
        df <- do.call("f", marginaleffects_numDeriv)
    }

    return(df)
}
