# Reorder a named covariance matrix to coefficient order. A matrix whose
# names are a permutation of the coefficient names is reordered; a matrix
# with complete unique names which are not a permutation is an error, because
# every downstream consumer multiplies positionally and equal dimensions would
# hide the mismatch. Unnamed or partially named matrices pass through: with no
# names there is nothing to align by, and rejecting them would break models
# whose vcov methods return bare matrices.
align_vcov_to_coef <- function(V, model, ...) {
    vnames <- colnames(V)
    if (is.null(vnames)) {
        warn_sprintf(
            "The supplied `vcov` matrix has no column names, so it is assumed to be in the same order as the model coefficients. Supply a matrix with coefficient names to have it checked and aligned."
        )
        return(V)
    }
    if (anyDuplicated(vnames) > 0L) {
        return(V)
    }
    beta <- tryCatch(get_coef(model, ...), error = function(e) NULL)
    bnames <- names(beta)
    if (is.null(bnames) || anyDuplicated(bnames) > 0L) {
        return(V)
    }
    rnames <- rownames(V)
    if (!is.null(rnames) && !identical(rnames, vnames)) {
        stop_sprintf(
            "The supplied variance-covariance matrix has row names which differ from its column names."
        )
    }
    if (setequal(vnames, bnames)) {
        if (!identical(vnames, bnames)) {
            # Index positionally by matched columns: row names may be absent,
            # and name-indexing rows would fail with an uninformative
            # subscript error.
            idx <- match(bnames, vnames)
            V <- V[idx, idx, drop = FALSE]
            dimnames(V) <- list(bnames, bnames)
        }
        return(V)
    }
    if (ncol(V) == length(beta)) {
        stop_sprintf(
            "The supplied variance-covariance matrix has the same dimension as the coefficient vector, but its names do not match the coefficient names."
        )
    }
    # A matrix which covers only a subset of the (non-aliased) coefficients
    # would silently treat the missing ones as known constants downstream.
    missing_names <- setdiff(bnames[!is.na(beta)], vnames)
    if (length(missing_names) > 0) {
        stop_sprintf(
            "The supplied variance-covariance matrix does not cover all model coefficients. Missing: %s",
            toString(missing_names)
        )
    }
    V
}


align_jacobian_vcov <- function(J, V, object, ...) {
    # Equal dimensions with matching name sets still need reordering: without
    # it the multiplication downstream is positional and silently wrong for a
    # permuted matrix.
    jnames <- colnames(J)
    vnames <- colnames(V)
    if (
        !is.null(jnames) && !is.null(vnames) &&
            anyDuplicated(jnames) == 0L && anyDuplicated(vnames) == 0L &&
            setequal(jnames, vnames)
    ) {
        if (!identical(jnames, vnames)) {
            V <- V[jnames, jnames, drop = FALSE]
        }
        return(list(J = J, V = V))
    }
    if (!isTRUE(ncol(J) == ncol(V))) {
        beta <- get_coef(object, ...)
        # Issue #718: ordinal::clm in test-pkg-ordinal.R
        if (
            anyNA(beta) &&
                ncol(J) > ncol(V) &&
                ncol(J) == length(beta) &&
                length(stats::na.omit(beta)) == ncol(V)
        ) {
            J <- J[, !is.na(beta), drop = FALSE]
        } else {
            cols <- intersect(colnames(J), colnames(V))
            if (length(cols) == 0) {
                stop_sprintf(
                    "The jacobian does not match the variance-covariance matrix."
                )
            }
            V <- V[cols, cols, drop = FALSE]
            J <- J[, cols, drop = FALSE]
        }
    }
    return(list(J = J, V = V))
}


std_error_from_jacobian <- function(J, V, object, ...) {
    # Covariance propagation is shared algebra. Eligibility for the analytic
    # Jacobian remains independent of this helper.
    jnames <- colnames(J)
    vnames <- colnames(V)
    if (
        !is.null(jnames) && !is.null(vnames) &&
            !identical(jnames, vnames) &&
            anyDuplicated(jnames) == 0L && anyDuplicated(vnames) == 0L &&
            setequal(jnames, vnames)
    ) {
        # Reorder only when the orders actually differ: subsetting an
        # already-aligned matrix copies p x p doubles for nothing.
        V <- V[jnames, jnames, drop = FALSE]
    }
    if (!isTRUE(ncol(J) == ncol(V))) {
        aligned <- align_jacobian_vcov(J, V, object, ...)
        J <- aligned$J
        V <- aligned$V
    }

    # Avoid constructing the full J V J' matrix when only its diagonal is used.
    variances <- rowSums(tcrossprod(J, V) * J)
    # tiny negative variances are floating-point noise: clamp them to zero
    scale <- suppressWarnings(max(abs(variances), na.rm = TRUE))
    if (!is.finite(scale)) {
        scale <- 0
    }
    tol <- sqrt(.Machine$double.eps) * max(1, scale)
    variances[variances < 0 & variances > -tol] <- 0
    se <- sqrt(variances)
    # A zero here is a statement, not a failure: a constant estimand -- a
    # contrast of a level with itself, a zero row of an exact Jacobian -- has
    # variance exactly zero. Test statistics on such rows are undefined and
    # come out NaN downstream, which is the correct separate signal. The
    # clamping above is deliberately kept separate: it removes negative noise
    # without turning an exact zero into a missing value.
    list(std.error = se, jacobian = J)
}


#' Jacobian of a delta method estimand with respect to model coefficients
#'
#' Covariance propagation is deliberately left to the caller: the unconditional
#' variance path needs this derivative without ever forming a coefficient
#' covariance matrix, and the plan-based delta method propagates the composed
#' derivative rather than this one.
#'
#' @inheritParams slopes
#' @param FUN a function which accepts a `model` and other inputs and returns a
#'   vector of estimates (marginal effects, marginal means, etc.)
#' @param vcov accepted and ignored, so that callers can build a single
#'   argument list for this function and for `get_se_delta()`.
#' @return `NULL` when the model admits no delta method, otherwise a list with
#'   the `jacobian`, the `method` which produced it ("custom" or "numeric"),
#'   and the `coefs` it was differentiated at.
#' @noRd
get_delta_jacobian <- function(
    model_perturbed,
    FUN,
    mfx = NULL,
    type = NULL,
    newdata = NULL,
    eps = NULL,
    J = NULL,
    hypothesis = NULL,
    calling_function = NULL,
    comparison = NULL,
    by = NULL,
    hi = NULL,
    lo = NULL,
    original = NULL,
    estimates = NULL,
    vcov = NULL,
    ...) {
    # Use mfx slots when available
    if (!is.null(mfx)) {
        eps <- if (is.null(eps)) mfx@eps else eps
        calling_function <- if (is.null(calling_function)) mfx@calling_function else calling_function
        comparison <- if (is.null(comparison)) mfx@comparison else comparison
        by <- if (is.null(by)) mfx@by else by
    }
    # delta method does not work for these models
    bad <- c("brmsfit", "stanreg", "bart")
    if (any(inherits(model_perturbed, bad))) {
        return(NULL)
    }

    coefs <- get_coef(model_perturbed, ...)

    # user-supplied jacobian machine
    if (is.null(J)) {
        fun <- settings_get("jacobian_function")
        if (is.null(fun)) {
            fun <- function(...) NULL
        }
        if (!"..." %in% names(formals(fun))) {
            msg <- "The `marginaleffects_jacobian_function` option must accept the ... argument."
            stop_sprintf(msg)
        }
        J <- fun(
            coefs = coefs,
            mfx = mfx,
            newdata = newdata,
            model_perturbed = model_perturbed,
            hypothesis = hypothesis,
            type = type,
            by = by,
            hi = hi,
            lo = lo,
            original = original,
            estimates = estimates,
            comparison = comparison,
            calling_function = calling_function
        )
        checkmate::assert_matrix(J, mode = "numeric", ncols = length(coefs), null.ok = TRUE)

        # A user-supplied jacobian function computes on `mfx@newdata`, which
        # may be padded; the reported results are not. Drop the padded rows.
        if (!is.null(J) && !is.null(mfx) && "rowid" %in% colnames(mfx@newdata)) {
            idx <- mfx@newdata$rowid > 0
            if (!all(idx) && nrow(J) == nrow(mfx@newdata)) {
                J <- J[idx, , drop = FALSE]
            }
        }
    }

    # input: named vector of coefficients
    # output: gradient
    inner <- function(x) {
        names(x) <- names(coefs)
        model_tmp <- set_coef(model_perturbed, x, ...)
        # do not pass NULL arguments. Important for `deltam` to allow users to supply FUN without ...
        args <- list(
            mfx = mfx,
            model_perturbed = model_tmp,
            hypothesis = hypothesis,
            type = type,
            hi = hi,
            lo = lo,
            original = original,
            by = by
        )
        args <- c(args, list(...))
        if (inherits(model_perturbed, "gamlss")) {
            args[["safe"]] <- FALSE
        }
        if (!is.null(eps)) {
            args[["eps"]] <- eps
        }
        if (!is.null(type)) {
            args[["type"]] <- type
        }
        if (!is.null(newdata)) {
            args[["newdata"]] <- newdata
        }
        if (!is.null(J)) {
            args[["J"]] <- J
        }
        if (!is.null(eps)) {
            args[["eps"]] <- eps
        }

        if (inherits(model_perturbed, "glmmTMB")) {
            args$newparams <- x
        }

        g <- do_call(FUN, args)

        return(g)
    }

    # A matrix returned by the user's `marginaleffects_jacobian_function` is
    # authoritative for the whole pipeline, hypothesis included: the function
    # receives the `hypothesis` argument above, so recomputing here would both
    # discard the user's work and misreport its provenance. Only a NULL return
    # falls back to numerical differentiation.
    method <- if (is.null(J)) "numeric" else "custom"
    if (is.null(J)) {
        args <- list(
            func = inner,
            x = coefs,
            numderiv = mfx@numderiv
        )
        J <- do.call("get_jacobian", args)
        colnames(J) <- names(coefs)
    }

    list(jacobian = J, method = method, coefs = coefs)
}


#' Compute standard errors using the delta method
#'
#' @inheritParams slopes
#' @param FUN a function which accepts a `model` and other inputs and returns a
#'   vector of estimates (marginal effects, marginal means, etc.)
#' @return vector of standard errors
#' @noRd
get_se_delta <- function(
    model_perturbed,
    vcov,
    FUN,
    mfx = NULL,
    type = NULL,
    newdata = NULL,
    eps = NULL,
    J = NULL,
    hypothesis = NULL,
    calling_function = NULL,
    comparison = NULL,
    by = NULL,
    hi = NULL,
    lo = NULL,
    original = NULL,
    estimates = NULL,
    ...) {
    jac <- get_delta_jacobian(
        model_perturbed = model_perturbed,
        FUN = FUN,
        mfx = mfx,
        type = type,
        newdata = newdata,
        eps = eps,
        J = J,
        hypothesis = hypothesis,
        calling_function = calling_function,
        comparison = comparison,
        by = by,
        hi = hi,
        lo = lo,
        original = original,
        estimates = estimates,
        ...
    )
    if (is.null(jac)) {
        return(NULL)
    }
    coefs <- jac$coefs

    # some vcov methods return an unnamed matrix, some have duplicate names
    flag <- anyDuplicated(colnames(vcov)) == 0 &&
        anyDuplicated(names(coefs)) == 0
    if (
        flag &&
            !is.null(dimnames(vcov)) &&
            all(names(coefs) %in% colnames(vcov))
    ) {
        bnames <- intersect(names(coefs), colnames(vcov))
        vcov <- vcov[bnames, bnames, drop = FALSE]
        colnames(vcov) <- row.names(vcov) <- names(coefs)
    }

    propagated <- std_error_from_jacobian(
        jac$jacobian,
        vcov,
        model_perturbed,
        ...
    )
    se <- propagated$std.error
    attr(se, "jacobian") <- propagated$jacobian
    attr(se, "jacobian_source") <- jac$method

    return(se)
}
