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
    list(cmp = cmp, hyp = hyp)
}
