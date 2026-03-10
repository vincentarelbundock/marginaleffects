get_jacobian <- function(func, x, numderiv) {
    numDeriv_options <- getOption("marginaleffects_numDeriv", default = NULL)
    if (is.null(numDeriv_options)) {
        method <- numderiv[[1]]
        numderiv[[1]] <- NULL
        numderiv[["func"]] <- func
        numderiv[["x"]] <- x
        if (identical(method, "richardson")) {
            df <- do.call(get_jacobian_richardson, numderiv)
        } else if (identical(method, "fdforward")) {
            df <- do.call(get_jacobian_fdforward, numderiv)
        } else if (identical(method, "fdcenter")) {
            df <- do.call(get_jacobian_fdcenter, numderiv)
        }
    } else {
        insight::check_if_installed("numDeriv")
        numDeriv_options[["func"]] <- func
        numDeriv_options[["x"]] <- x
        ndFUN <- get("jacobian", asNamespace("numDeriv"))
        df <- do.call(ndFUN, numDeriv_options)
    }
    return(df)
}


get_jacobian_fdforward <- function(func, x, eps = NULL) {
    # old version. probably not optimal. Keep for posterity.
    # h <- max(1e-8, 1e-4 * min(abs(x), na.rm = TRUE))
    baseline <- func(x)

    # we pre-chunk because future does not cache datasets in nodes, which means we
    # must pass every worker the full data and model for every future.
    inner_loop <- function(chunk, ...) {
        out <- list()
        for (i in chunk) {
            if (is.null(eps)) {
                h <- max(abs(x[i]) * sqrt(.Machine$double.eps), 1e-10)
            } else {
                h <- eps
            }
            dx <- x
            dx[i] <- dx[i] + h
            out <- c(out, list((func(dx) - baseline) / h))
        }
        out <- do.call(cbind, out)
        return(out)
    }

    if (isTRUE(getOption("marginaleffects_parallel", default = FALSE))) {
        insight::check_if_installed("future.apply")
        insight::check_if_installed("future")
        insight::check_if_installed("parallel")
        chunks <- parallel::splitIndices(length(x), future::nbrOfWorkers())
        df <- future.apply::future_lapply(
            chunks,
            inner_loop,
            future.seed = TRUE
        )
        df <- do.call("cbind", df)
    } else {
        chunks <- seq_along(x)
        df <- lapply(chunks, inner_loop)
        df <- do.call("cbind", df)
    }

    return(df)
}


get_jacobian_fdcenter <- function(func, x, eps = NULL) {
    baseline <- func(x)
    # we pre-chunk because future does not cache datasets in nodes, which means we
    # must pass every worker the full data and model for every future.
    inner_loop <- function(chunk, ...) {
        out <- list()
        for (i in chunk) {
            if (is.null(eps)) {
                h <- max(abs(x[i]) * sqrt(.Machine$double.eps), 1e-10)
            } else {
                h <- eps
            }
            dx_hi <- dx_lo <- x
            dx_hi[i] <- dx_hi[i] + h / 2
            dx_lo[i] <- dx_lo[i] - h / 2
            out <- c(out, list((func(dx_hi) - func(dx_lo)) / h))
        }
        out <- do.call(cbind, out)
        return(out)
    }

    if (isTRUE(getOption("marginaleffects_parallel", default = FALSE))) {
        insight::check_if_installed("future.apply")
        insight::check_if_installed("future")
        insight::check_if_installed("parallel")
        chunks <- parallel::splitIndices(length(x), future::nbrOfWorkers())
        df <- future.apply::future_lapply(
            chunks,
            inner_loop,
            future.seed = TRUE
        )
        df <- do.call("cbind", df)
    } else {
        chunks <- seq_along(x)
        df <- lapply(chunks, inner_loop)
        df <- do.call("cbind", df)
    }
    return(df)
}


# Code adapted from the `numDeriv` package by Paul Gilbert and Ravi Varadhan
# GPL-3: https://cran.r-project.org/package=numDeriv
get_jacobian_richardson <- function(
    func,
    x,
    eps = 1e-4,
    d = 1e-4,
    zero_tol = sqrt(.Machine$double.eps / 7e-7),
    side = NULL,
    r = 4,
    v = 2,
    ...
) {
    n <- length(x)
    f <- func(x, ...)

    a <- array(NA, c(length(f), r, n))
    h <- abs(d * x) + eps * (abs(x) < zero_tol)
    pna <- (side == 1) & !is.na(side) # double these on plus side
    mna <- (side == -1) & !is.na(side) # double these on minus side
    for (k in 1:r) {
        # successively reduce h
        ph <- mh <- h
        ph[pna] <- 2 * ph[pna]
        ph[mna] <- 0
        mh[mna] <- 2 * mh[mna]
        mh[pna] <- 0
        for (i in 1:n) {
            a[, k, i] <- (func(x + ph * (i == seq(n)), ...) -
                func(x - mh * (i == seq(n)), ...)) /
                (2 * h[i])
            # if((k != 1)) a[,(abs(a[,(k-1),i]) < 1e-20)] <- 0 #some func are unstable near zero
        }
        h <- h / v # Reduced h by 1/v.
    }
    for (m in 1:(r - 1)) {
        a <- (a[, 2:(r + 1 - m), , drop = FALSE] *
            (4^m) -
            a[, 1:(r - m), , drop = FALSE]) /
            (4^m - 1)
    }
    # drop second dim of a, which is now 1 (but not other dim's even if they are 1
    return(array(a, dim(a)[c(1, 3)]))
}
