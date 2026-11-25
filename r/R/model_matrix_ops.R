# Model matrix operations shared by dense matrices and lazy designs.
#
# The cached model matrix attached to `newdata` is either a dense numeric
# matrix or, for eligible models (exact classes "lm" and "glm", full rank,
# no offset) when the optional design.matrix package is installed, a
# `design_matrix` object: the factor codes and numeric variables, from which
# products are computed without forming the n x p matrix.
#
# The analytic Jacobian code is written once for both. Base syntax already
# works on either kind -- `X %*% beta`, row scaling `d * X`, combinations
# `w_hi * X_hi + w_lo * X_lo`, row selection `X[i, ]`, `nrow()`, `colnames()`
# and `as.matrix()` -- so a Jacobian built from designs is itself a
# (combination) design. The helpers below cover the few operations whose
# spelling differs between the two kinds.


design_matrix_enabled <- function() {
    isTRUE(getOption("marginaleffects_design_matrix", default = TRUE)) &&
        requireNamespace("design.matrix", quietly = TRUE)
}


is_design_matrix <- function(x) {
    inherits(x, "design_matrix")
}


# A usable cached model matrix: a dense numeric matrix or a design.
is_model_matrix_cache <- function(x) {
    isTRUE(checkmate::check_matrix(x)) || is_design_matrix(x)
}


# The design of `newdata` for an eligible model, or NULL.
design_matrix_build <- function(model, newdata) {
    if (!design_matrix_enabled()) {
        return(NULL)
    }
    if (!isTRUE(class(model)[1] %in% c("lm", "glm"))) {
        return(NULL)
    }
    beta <- stats::coef(model)
    # Rank-deficient fits pivot their columns in get_predict.lm(), and a
    # design cannot reorder its columns: keep those on the dense path.
    if (anyNA(beta) || !isTRUE(model$rank == length(beta))) {
        return(NULL)
    }
    D <- design.matrix::design_matrix(model, newdata = newdata)
    if (!identical(colnames(D), names(beta))) {
        return(NULL)
    }
    D
}


# The dense matrix of either kind; dense matrices pass through. A design's
# dense matrix keeps only its dimensions and column names, as the Jacobian
# contract requires (no row names, `assign` or `contrasts`).
mm_dense <- function(x) {
    if (!is_design_matrix(x)) {
        return(x)
    }
    out <- as.matrix(x)
    attributes(out) <- list(dim = dim(out), dimnames = list(NULL, colnames(out)))
    out
}


# Are all the entries of a model matrix finite? `eta` is X %*% beta for
# finite beta. A dense matrix is scanned. A design is judged by `eta`: its
# kernels list every column of a row with a non-finite input, so any
# non-finite entry makes that row of `eta` non-finite (Inf * 0 is NaN), and
# the only false rejection is a finite matrix whose linear predictor
# overflows, which fails in the safe direction.
mm_all_finite <- function(X, eta) {
    if (is_design_matrix(X)) all(is.finite(eta)) else matrix_all_finite(X)
}


# A design's row scales are finite (a non-finite scale makes its rows
# non-finite in the dense matrix). Dense matrices are checked by the caller.
mm_scales_finite <- function(J) {
    if (!is_design_matrix(J)) {
        return(TRUE)
    }
    for (s in list(J[["scale"]], J[["scale2"]])) {
        if (!is.null(s) && !all(is.finite(s))) {
            return(FALSE)
        }
    }
    TRUE
}


# t(W) %*% M for a sparse weight matrix W with entries (rows, cols, vals),
# n_out columns: one row per output, each a weighted sum of rows of M.
#
# Dense: a single output is one weighted column sum, which crossprod() forms
# without building any matrix at all; several outputs are one sparse
# contraction, a single pass over M rather than one subset per group.
# Design: a weight vector over its rows, contracted by crossprod() or by one
# grouped column sum. A row feeding several outputs is first repeated with
# `[`, so that each row of the contracted design feeds one output.
mm_group_sums <- function(M, n_out, rows, cols, vals) {
    n_out <- as.integer(n_out)
    if (is_design_matrix(M)) {
        if (n_out > 1L && anyDuplicated(rows) > 0L) {
            M <- M[rows, ]
            rows <- seq_along(rows)
        }
        n <- nrow(M)
        v <- numeric(n)
        if (n_out == 1L) {
            if (anyDuplicated(rows) > 0L) {
                s <- rowsum(vals, rows, reorder = FALSE)
                v[as.integer(rownames(s))] <- s[, 1L]
            } else {
                v[rows] <- vals
            }
            return(matrix(as.vector(crossprod(M, v)), nrow = 1L))
        }
        v[rows] <- vals
        g <- rep.int(cols[1L], n)
        g[rows] <- cols
        S <- design.matrix::colSums(v * M, group = g)
        out <- matrix(0, nrow = n_out, ncol = ncol(M))
        out[as.integer(rownames(S)), ] <- unname(S)
        return(out)
    }
    if (n_out == 1L) {
        sub <- if (identical(rows, seq_len(nrow(M)))) M else M[rows, , drop = FALSE]
        return(matrix(drop(crossprod(vals, sub)), nrow = 1L))
    }
    W <- Matrix::sparseMatrix(
        i = rows,
        j = cols,
        x = vals,
        dims = c(nrow(M), n_out)
    )
    as.matrix(Matrix::crossprod(W, M))
}


# diag(J V J'), the variances of the estimates, without forming J V J'.
# V must already be aligned with the columns of J.
mm_quad_diag <- function(J, V) {
    if (is_design_matrix(J) && all(is.finite(V))) {
        return(design.matrix::quadratic_diagonal(J, unname(V)))
    }
    J <- mm_dense(J)
    rowSums(tcrossprod(J, V) * J)
}
