# Extract the coefficient vector of a strictly linear hypothesis expression,
# by structural recursion on its syntax tree.
#
# This is a proof, not a probe: a parse tree built only from estimate
# references, numeric constants, sums and differences, products with a
# constant side, and division by a constant denotes a linear map as a matter
# of syntax, wherever it is evaluated. Anything else -- unknown symbols,
# function calls, a product of two estimate terms -- yields NULL. No finite
# set of numeric evaluations can establish this property for arbitrary code,
# because a function can be built to agree with any fixed evaluation points
# and disagree elsewhere; the syntactic argument has no such gap. And the
# same structural walk which proves the map linear also reads off its
# coefficients, so the contrast matrix comes from the proof itself rather
# than from re-evaluating the closure against basis vectors.
#
# Returns a list with a `coef` vector of length `n_estimates` aligned to
# estimate positions and the additive `const`, or NULL when the expression is
# not a valid combination. Strict linearity is `const == 0`; an affine map
# has a derivative but no crossprod(H, estimate) representation, so callers
# must not promote it.
#
# `labels` and `idx` are parallel: `labels[k]` is the symbol which stands for
# estimate position `idx[k]`.
hypothesis_expression_coefficients <- function(expr, labels, idx, n_estimates) {
    position <- stats::setNames(as.integer(idx), labels)

    const_value <- function(e) {
        v <- tryCatch(eval(e, baseenv()), error = function(err) NULL)
        if (is.numeric(v) && length(v) == 1L && is.finite(v)) {
            return(v)
        }
        NULL
    }

    walk <- function(e) {
        if (is.numeric(e) && length(e) == 1L && is.finite(e)) {
            return(list(const = as.numeric(e), coef = numeric(n_estimates)))
        }
        if (is.symbol(e)) {
            pos <- position[[as.character(e)]]
            if (is.null(pos) || is.na(pos)) {
                return(NULL)
            }
            coef <- numeric(n_estimates)
            coef[pos] <- 1
            return(list(const = 0, coef = coef))
        }
        if (!is.call(e) || length(e) < 2L || !is.symbol(e[[1L]])) {
            return(NULL)
        }
        op <- as.character(e[[1L]])
        if (op == "(" && length(e) == 2L) {
            return(walk(e[[2L]]))
        }
        if (op %in% c("+", "-") && length(e) == 2L) {
            a <- walk(e[[2L]])
            if (is.null(a)) {
                return(NULL)
            }
            if (op == "-") {
                a$const <- -a$const
                a$coef <- -a$coef
            }
            return(a)
        }
        if (op %in% c("+", "-") && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (is.null(a) || is.null(b)) {
                return(NULL)
            }
            s <- if (op == "+") 1 else -1
            return(list(
                const = a$const + s * b$const,
                coef = a$coef + s * b$coef
            ))
        }
        if (op == "*" && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (is.null(a) || is.null(b)) {
                return(NULL)
            }
            a_const <- all(a$coef == 0)
            b_const <- all(b$coef == 0)
            if (a_const && b_const) {
                return(list(const = a$const * b$const, coef = a$coef))
            }
            if (a_const) {
                return(list(const = a$const * b$const, coef = a$const * b$coef))
            }
            if (b_const) {
                return(list(const = a$const * b$const, coef = b$const * a$coef))
            }
            return(NULL)
        }
        if (op == "/" && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (is.null(a) || is.null(b) || !all(b$coef == 0) || b$const == 0) {
                return(NULL)
            }
            return(list(const = a$const / b$const, coef = a$coef / b$const))
        }
        if (op == "^" && length(e) == 3L) {
            a <- walk(e[[2L]])
            b <- walk(e[[3L]])
            if (
                is.null(a) || is.null(b) ||
                    !all(a$coef == 0) || !all(b$coef == 0)
            ) {
                return(NULL)
            }
            v <- a$const^b$const
            if (!is.finite(v)) {
                return(NULL)
            }
            return(list(const = v, coef = a$coef))
        }
        NULL
    }

    exprs <- as.list(expr)
    if (length(exprs) != 1L) {
        return(NULL)
    }
    e <- exprs[[1L]]
    # Top-level "lhs = rhs" is the null-hypothesis form; the map is the lhs
    # and the rhs must be a constant which is exactly zero, because the
    # closure computing the estimates evaluates "lhs - (rhs)".
    if (
        is.call(e) && length(e) == 3L && is.symbol(e[[1L]]) &&
            identical(as.character(e[[1L]]), "=")
    ) {
        rhs <- const_value(e[[3L]])
        if (is.null(rhs) || rhs != 0) {
            return(NULL)
        }
        e <- e[[2L]]
    }
    out <- walk(e)
    if (is.null(out) || !all(is.finite(out$coef)) || !is.finite(out$const)) {
        return(NULL)
    }
    out
}


# A hypothesis expressed as a string is very often a linear map of the
# estimates, even when it is stored as an opaque closure. A linear map has an
# exact matrix representation, which is faster than any probe, exact rather
# than approximate, and consumable by the analytic Jacobian. Record the
# hypothesis as a matrix stage when the syntax tree proves linearity; leave
# the closure in place so estimates are computed exactly as before, and keep
# every other list element and attribute untouched.
#
# `H` is the matrix compiled directly from the syntax tree by the caller: the
# same walk which proves the map linear also reads off its coefficients, so
# no basis-vector probing of the closure is involved. User-supplied function
# hypotheses and formula closures never reach here -- probing cannot prove
# linearity of arbitrary code, so they stay on the composed-derivative path,
# which differentiates them at the estimate instead of extrapolating a
# structure they were never proven to have. The only runtime check retained
# is one matrix-vector product confirming that H reproduces the estimates the
# closure computed, which guards the compiler itself.
hypothesis_promote_matrix <- function(hyp, cmp_skeleton, H = NULL) {
    if (is.null(H)) {
        return(hyp)
    }
    if (!isTRUE(getOption("marginaleffects_hypothesis_promote", default = TRUE))) {
        return(hyp)
    }
    if (is.null(hyp) || identical(hyp$kind, "matrix") || !is.function(hyp$apply)) {
        return(hyp)
    }
    estimate <- cmp_skeleton[["estimate"]]
    if (!is.numeric(estimate) || nrow(H) != length(estimate)) {
        return(hyp)
    }
    want <- drop(crossprod(H, estimate))
    base <- tryCatch(hyp$apply(estimate), error = function(e) NULL)
    if (
        !is.numeric(base) || length(base) != ncol(H) || anyNA(base) ||
            max(abs(want - base)) > 1e-9 * max(1, max(abs(base)))
    ) {
        return(hyp)
    }
    out <- hyp
    out$kind <- "matrix"
    out$H <- H
    # Copy attributes individually: replacing the attribute list wholesale
    # would restore the old `names` and lose the new element.
    for (nm in setdiff(names(attributes(hyp)), "names")) {
        attr(out, nm) <- attr(hyp, nm)
    }
    out
}


hypothesis_compile <- function(hypothesis, cmp_skeleton, by = NULL, newdata = NULL, mfx = NULL) {
    if (is.null(hypothesis)) {
        return(list(cmp = cmp_skeleton, hyp = NULL))
    }

    draws <- attr(cmp_skeleton, "posterior_draws")
    vec <- isTRUE(checkmate::check_atomic_vector(hypothesis)) &&
        isTRUE(checkmate::check_numeric(hypothesis))
    mat <- isTRUE(checkmate::check_matrix(hypothesis))

    if (!is.null(draws)) {
        return(hypothesis_compile_wrapper(hypothesis, cmp_skeleton, by, newdata, mfx))
    }

    if (vec || mat) {
        return(hypothesis_compile_matrix(hypothesis, cmp_skeleton))
    }

    if (is.character(hypothesis)) {
        return(hypothesis_compile_string(hypothesis, cmp_skeleton))
    }

    if (isTRUE(checkmate::check_formula(hypothesis))) {
        return(hypothesis_compile_formula(hypothesis, cmp_skeleton, by, newdata, mfx))
    }

    hypothesis_compile_wrapper(hypothesis, cmp_skeleton, by, newdata, mfx)
}

hypothesis_compile_wrapper <- function(hypothesis, cmp_skeleton, by, newdata, mfx) {
    skeleton <- data.table::copy(cmp_skeleton)
    apply_df <- function(est) {
        x <- data.table::copy(skeleton)
        x[["estimate"]] <- est
        get_hypothesis(
            x,
            hypothesis = hypothesis,
            by = by,
            newdata = newdata,
            draws = NULL,
            mfx = mfx
        )
    }
    cmp <- apply_df(skeleton[["estimate"]])
    if (!isTRUE(checkmate::check_numeric(cmp[["estimate"]]))) {
        msg <- "The `hypothesis` argument must produce numeric estimates."
        stop(msg, call. = FALSE)
    }
    hyp <- list(
        kind = "wrapper",
        apply = function(est) apply_df(est)[["estimate"]]
    )
    attr(hyp, "hypothesis_function_by") <- attr(cmp, "hypothesis_function_by")
    # Never promoted: the wrapper wraps arbitrary user code, whose linearity
    # cannot be proven.
    list(cmp = cmp, hyp = hyp)
}

hypothesis_compile_formula <- function(hypothesis, cmp_skeleton, by, newdata, mfx) {
    form <- sanitize_hypothesis_formula(hypothesis)

    cmp <- hypothesis_formula(
        data.table::copy(cmp_skeleton),
        hypothesis = hypothesis,
        newdata = newdata,
        by = by,
        mfx = mfx
    )

    if (!isTRUE(checkmate::check_numeric(cmp[["estimate"]]))) {
        msg <- "The `hypothesis` argument must produce numeric estimates."
        stop(msg, call. = FALSE)
    }

    if (isTRUE(form$lhs == "arbitrary_function")) {
        fun_comparison <- sprintf("function(x) %s", form$rhs)
        fun_comparison <- eval(parse(text = fun_comparison), envir = environment(hypothesis))
    } else {
        fun_comparison <- hypothesis_formula_list[[form$rhs]][[form$lhs]]$comparison
    }

    if (is.null(form$group)) {
        x <- cmp_skeleton
    } else {
        group_cols <- intersect(form$group, colnames(cmp_skeleton))
        x <- as_data_table_select(cmp_skeleton, group_cols)
    }
    groupval <- hypothesis_formula_groups(x, newdata, form$group)

    if (is.null(groupval)) {
        groups <- list(seq_len(nrow(x)))
    } else {
        combined <- data.table::data.table(
            marginaleffects_formula_idx = seq_len(nrow(cmp_skeleton))
        )
        combined <- cbind(combined, groupval)
        data.table::setDT(combined)
        groups <- combined[
            ,
            .(idx = list(marginaleffects_formula_idx)),
            keyby = groupval
        ][["idx"]]
    }

    apply <- function(est) {
        unlist(
            lapply(groups, function(idx) fun_comparison(est[idx])),
            use.names = FALSE
        )
    }

    H <- hypothesis_compile_formula_matrix(form, groups)
    if (!is.null(H) && ncol(H) == nrow(cmp)) {
        apply <- function(est) as.vector(Matrix::crossprod(H, est))
        hyp <- list(kind = "matrix", apply = apply, H = H)
    } else {
        # Not promoted: the linear formula shortcuts already produced their
        # matrix syntactically above, so whatever reaches this branch -- a
        # ratio shortcut, an arbitrary-function right-hand side -- has no
        # proven linear structure.
        hyp <- list(kind = "formula", apply = apply)
    }
    attr(hyp, "hypothesis_function_by") <- attr(cmp, "hypothesis_function_by")
    list(cmp = cmp, hyp = hyp)
}

hypothesis_compile_formula_matrix <- function(form, groups) {
    if (!isTRUE(form$lhs %in% c("difference", "dotproduct"))) {
        return(NULL)
    }

    group_sizes <- lengths(groups)
    unique_sizes <- unique(group_sizes)
    block_cache <- tryCatch(
        lapply(unique_sizes, function(n) {
            hypothesis_compile_formula_matrix_block(form$rhs, n)
        }),
        error = function(e) NULL
    )
    if (is.null(block_cache) || any(vapply(block_cache, is.null, logical(1)))) {
        return(NULL)
    }

    blocks <- block_cache[match(group_sizes, unique_sizes)]
    H <- Matrix::bdiag(blocks)
    row_order <- unlist(groups, use.names = FALSE)
    H[match(seq_along(row_order), row_order), , drop = FALSE]
}

hypothesis_compile_formula_matrix_block <- function(shortcut, n) {
    if (shortcut %in% c("reference", "revreference")) {
        if (n < 2L) {
            return(NULL)
        }
        s <- if (shortcut == "reference") 1 else -1
        return(Matrix::sparseMatrix(
            i = c(seq.int(2L, n), rep.int(1L, n - 1L)),
            j = rep.int(seq_len(n - 1L), 2L),
            x = c(rep.int(s, n - 1L), rep.int(-s, n - 1L)),
            dims = c(n, n - 1L)
        ))
    }

    if (shortcut == "sequential") {
        if (n < 2L) {
            return(NULL)
        }
        return(Matrix::sparseMatrix(
            i = c(seq.int(2L, n), seq_len(n - 1L)),
            j = rep.int(seq_len(n - 1L), 2L),
            x = rep(c(1, -1), each = n - 1L),
            dims = c(n, n - 1L)
        ))
    }

    if (shortcut %in% c("pairwise", "revpairwise")) {
        if (shortcut == "pairwise") {
            pairs <- which(lower.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
        } else {
            pairs <- which(upper.tri(matrix(FALSE, n, n)), arr.ind = TRUE)
        }
        npairs <- nrow(pairs)
        return(Matrix::sparseMatrix(
            i = c(pairs[, 1L], pairs[, 2L]),
            j = rep.int(seq_len(npairs), 2L),
            x = rep(c(1, -1), each = npairs),
            dims = c(n, npairs)
        ))
    }

    if (shortcut == "trt_vs_ctrl") {
        if (n < 2L) {
            return(NULL)
        }
        return(Matrix::sparseMatrix(
            i = seq_len(n),
            j = rep.int(1L, n),
            x = c(-1, rep.int(1 / (n - 1L), n - 1L)),
            dims = c(n, 1L)
        ))
    }

    if (shortcut == "meandev") {
        H <- diag(n) - matrix(1 / n, nrow = n, ncol = n)
        return(Matrix::Matrix(H, sparse = TRUE))
    }

    if (shortcut == "meanotherdev") {
        if (n < 2L) {
            return(NULL)
        }
        H <- diag(n) - matrix(1 / (n - 1L), nrow = n, ncol = n)
        diag(H) <- 1
        return(Matrix::Matrix(H, sparse = TRUE))
    }

    if (shortcut == "poly") {
        H <- stats::contr.poly(n)
        H <- H[, seq_len(min(5L, ncol(H))), drop = FALSE]
        return(Matrix::Matrix(H, sparse = TRUE))
    }

    if (shortcut == "helmert") {
        return(Matrix::Matrix(stats::contr.helmert(n), sparse = TRUE))
    }

    NULL
}

hypothesis_compile_matrix <- function(hypothesis, cmp_skeleton) {
    H <- hypothesis
    if (isTRUE(checkmate::check_atomic_vector(H))) {
        H <- matrix(H, ncol = 1)
    }
    checkmate::assert_matrix(H, nrows = nrow(cmp_skeleton))
    if (is.null(colnames(H))) {
        colnames(H) <- rep("custom", ncol(H))
    }
    terms <- colnames(H)
    keep <- terms != "1 - 1"
    apply <- function(est) {
        as.vector(est %*% H)[keep]
    }
    cmp <- data.table::data.table(
        term = terms[keep],
        estimate = apply(cmp_skeleton[["estimate"]])
    )
    hyp <- list(kind = "matrix", apply = apply, H = H[, keep, drop = FALSE])
    list(cmp = cmp, hyp = hyp)
}

hypothesis_compile_string <- function(hypothesis, cmp_skeleton) {
    cmp <- hypothesis_string(cmp_skeleton, hypothesis)
    eval_parent <- .GlobalEnv

    lab <- attr(hypothesis, "label")
    if (is.null(lab)) {
        lab <- hypothesis
    }
    expanded <- expand_wildcard(hypothesis, nrow(cmp_skeleton), lab)
    hyps <- expanded[[1]]
    labs <- expanded[[2]]

    compiled <- vector("list", length(hyps))
    for (i in seq_along(hyps)) {
        hyp <- hyps[[i]]
        positional <-
            isTRUE(grepl("\\bb\\d+\\b", hyp)) &&
                !any(grepl("\\bb\\d+\\b", cmp_skeleton[["term"]]))
        compiled[[i]] <- hypothesis_string_compile_expression(
            hyp,
            rowlabels = if (isTRUE(positional)) NULL else cmp_skeleton$term,
            n_estimates = nrow(cmp_skeleton),
            positional = positional,
            eval_parent = eval_parent
        )
    }

    apply <- function(est) {
        unlist(lapply(compiled, function(expr) {
            hypothesis_string_eval_compiled(est, expr)
        }), use.names = FALSE)
    }

    hyp <- list(kind = "string", apply = apply)
    # Promotion requires a syntactic proof of linearity for every compiled
    # expression, never a numeric probe: the walk which proves the shape also
    # compiles the contrast matrix, column by column. Any expression outside
    # the strictly linear fragment (an additive constant, a product of
    # estimates, a function call) yields NULL and leaves the hypothesis as a
    # closure.
    n_estimates <- nrow(cmp_skeleton)
    columns <- lapply(compiled, function(cc) {
        out <- hypothesis_expression_coefficients(
            cc$expr,
            cc$labels,
            cc$idx,
            n_estimates
        )
        if (is.null(out) || out$const != 0) {
            return(NULL)
        }
        out$coef
    })
    H <- NULL
    if (length(columns) > 0L && !any(vapply(columns, is.null, logical(1)))) {
        H <- do.call(cbind, columns)
    }
    hyp <- hypothesis_promote_matrix(hyp, cmp_skeleton, H = H)
    list(cmp = cmp, hyp = hyp)
}
