# adapted from the numDeriv package for R by Paul Gilbert published under GPL2 license

get_jacobian <- function(func, x) {
    numDeriv_args <- getOption("marginaleffects_numDeriv", default = NULL)

    # forward finite difference (faster)
    if (is.null(numDeriv_args)) {
        eps <- max(1e-8, 1e-4 * min(abs(x), na.rm = TRUE))
        baseline <- func(x)
        df <- matrix(NA_real_, length(baseline), length(x))
        for (i in seq_along(x)) {
            dx <- x
            dx[i] <- dx[i] + eps
            df[, i] <- (func(dx) - baseline) / eps
        }
        
        
    # numDeriv (more accurate)
    } else {
        insight::check_if_installed("numDeriv")
        numDeriv_args[["func"]] <- func
        numDeriv_args[["x"]] <- x
        ndFUN <- get("jacobian", asNamespace("numDeriv"))
        df <- do.call(ndFUN, numDeriv_args)
    }

    return(df)
}


# Code adapted from the `numDeriv` package by Paul Gilbert and Ravi Varadhan
# GPL-2: https://cran.r-project.org/package=numDeriv
get_jacobian_richardson <- function(
    func,
    x,
    eps = 1e-4,
    d = 1e-4,
    zero_tol = sqrt(.Machine$double.eps / 7e-7),
    side = NULL,
    r = 4,
    v = 2,
    ...) {

    n <- length(x)
    f <- func(x, ...)

    a <- array(NA, c(length(f), r, n))
    h <- abs(d * x) + eps * (abs(x) < zero_tol)
    pna <- (side == 1) & !is.na(side) # double these on plus side
    mna <- (side == -1) & !is.na(side) # double these on minus side
    for (k in 1:r) { # successively reduce h
        ph <- mh <- h
        ph[pna] <- 2 * ph[pna]
        ph[mna] <- 0
        mh[mna] <- 2 * mh[mna]
        mh[pna] <- 0
        for (i in 1:n) {
            a[, k, i] <- (func(x + ph * (i == seq(n)), ...) -
                func(x - mh * (i == seq(n)), ...)) / (2 * h[i])
            # if((k != 1)) a[,(abs(a[,(k-1),i]) < 1e-20)] <- 0 #some func are unstable near zero
        }
        h <- h / v # Reduced h by 1/v.
    }
    for (m in 1:(r - 1)) {
        a <- (a[, 2:(r + 1 - m), , drop = FALSE] * (4^m) - a[, 1:(r - m), , drop = FALSE]) / (4^m - 1)
    }
    # drop second dim of a, which is now 1 (but not other dim's even if they are 1
    return(array(a, dim(a)[c(1, 3)]))
}
