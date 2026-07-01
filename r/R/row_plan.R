rowplan_assert_draws <- function(out, draws, context = "row plan") {
    if (!is.null(draws) && nrow(out) != nrow(draws)) {
        msg <- sprintf(
            paste(
                "%s produced %s row(s), but the posterior draw matrix has",
                "%s row(s). This is an internal row alignment error."
            ),
            context,
            nrow(out),
            nrow(draws)
        )
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}


rowplan_assert_same_nrow <- function(before, after, context = "row plan") {
    if (nrow(before) != nrow(after)) {
        msg <- sprintf(
            "%s changed row count from %s to %s.",
            context,
            nrow(before),
            nrow(after)
        )
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}


rowplan_filter <- function(out, draws, idx, context = "row filter") {
    out <- out[idx, , drop = FALSE]
    if (!is.null(draws)) {
        draws <- draws[idx, , drop = FALSE]
    }
    rowplan_assert_draws(out, draws, context)
    return(list(out = out, draws = draws))
}


rowplan_groups <- function(n, groupval = NULL) {
    if (!inherits(groupval, "data.frame") || nrow(groupval) != n) {
        return(data.table::data.table(.rows = list(seq_len(n))))
    }

    out <- data.table::data.table(groupval)
    out[[".marginaleffects_rowid"]] <- seq_len(n)
    bycols <- setdiff(colnames(out), ".marginaleffects_rowid")
    out <- out[, .(.rows = list(.marginaleffects_rowid)), keyby = bycols]

    return(out)
}


rowplan_hypothesis_formula_draws <- function(draws, form, groupval, fun_comparison) {
    if (isTRUE(form$lhs == "arbitrary_function")) {
        return(NULL)
    }

    sentinel <- rowplan_hypothesis_sentinel(nrow(draws))
    reference <- matrix_apply_column(sentinel, FUN = fun_comparison, by = groupval)
    candidate <- rowplan_hypothesis_formula_draws_raw(
        draws = sentinel,
        form = form,
        groupval = groupval
    )
    order <- rowplan_hypothesis_sentinel_order(reference, candidate)
    if (is.null(order)) {
        return(NULL)
    }

    out <- rowplan_hypothesis_formula_draws_raw(
        draws = draws,
        form = form,
        groupval = groupval
    )
    if (is.null(out)) {
        return(NULL)
    }

    out <- out[order, , drop = FALSE]
    rowplan_assert_draws(reference, out, "hypothesis_formula sentinel plan")

    return(out)
}


rowplan_hypothesis_formula_draws_raw <- function(draws, form, groupval = NULL) {
    spec <- rowplan_hypothesis_formula_spec(form$rhs)
    if (is.null(spec) || !form$lhs %in% spec[["lhs"]]) {
        return(NULL)
    }

    groups <- rowplan_groups(nrow(draws), groupval = groupval)
    out <- vector("list", nrow(groups))

    for (i in seq_len(nrow(groups))) {
        rows <- groups[[".rows"]][[i]]
        if (length(rows) < spec[["min_n"]]) {
            return(NULL)
        }
        out[[i]] <- spec[["draws"]](draws[rows, , drop = FALSE], form$lhs)
        if (is.null(out[[i]])) {
            return(NULL)
        }
    }

    out <- do.call(rbind, out)

    return(out)
}


rowplan_hypothesis_sentinel <- function(n) {
    i <- seq_len(n)
    cbind(
        i + 0.125,
        log1p(i) + 0.25,
        sqrt(i) + 0.5,
        (i^2 + 1) / (n + 1)
    )
}


rowplan_hypothesis_sentinel_order <- function(reference, candidate) {
    if (is.null(candidate) || !identical(dim(reference), dim(candidate))) {
        return(NULL)
    }
    if (isTRUE(all.equal(reference, candidate, check.attributes = FALSE))) {
        return(seq_len(nrow(reference)))
    }

    key_reference <- rowplan_hypothesis_sentinel_key(reference)
    key_candidate <- rowplan_hypothesis_sentinel_key(candidate)

    if (anyDuplicated(key_reference) || anyDuplicated(key_candidate)) {
        return(NULL)
    }

    idx <- match(key_reference, key_candidate)
    if (anyNA(idx)) {
        return(NULL)
    }

    ordered <- candidate[idx, , drop = FALSE]
    if (!isTRUE(all.equal(reference, ordered, check.attributes = FALSE))) {
        return(NULL)
    }

    return(idx)
}


rowplan_hypothesis_sentinel_key <- function(x) {
    x <- signif(x, 12)
    apply(x, 1, paste, collapse = "\r")
}


rowplan_hypothesis_formula_spec <- function(rhs) {
    binary <- function(kind, min_n = 2L) {
        list(
            lhs = c("difference", "ratio"),
            min_n = min_n,
            draws = function(draws, lhs) {
                idx <- rowplan_hypothesis_binary_index(nrow(draws), kind)
                rowplan_hypothesis_binary_draws(draws, idx[["i"]], idx[["j"]], lhs)
            }
        )
    }

    switch(
        rhs,
        reference = binary("reference"),
        revreference = binary("revreference"),
        sequential = binary("sequential"),
        pairwise = binary("pairwise", min_n = 1L),
        revpairwise = binary("revpairwise", min_n = 1L),
        trt_vs_ctrl = list(
            lhs = c("difference", "ratio"),
            min_n = 2L,
            draws = rowplan_hypothesis_trt_vs_ctrl_draws
        ),
        meandev = list(
            lhs = c("difference", "ratio"),
            min_n = 1L,
            draws = rowplan_hypothesis_meandev_draws
        ),
        meanotherdev = list(
            lhs = c("difference", "ratio"),
            min_n = 2L,
            draws = rowplan_hypothesis_meanotherdev_draws
        ),
        poly = list(
            lhs = "dotproduct",
            min_n = 2L,
            draws = rowplan_hypothesis_poly_draws
        ),
        helmert = list(
            lhs = "dotproduct",
            min_n = 2L,
            draws = rowplan_hypothesis_helmert_draws
        ),
        NULL
    )
}


rowplan_hypothesis_binary_index <- function(n, kind) {
    if (kind == "reference") {
        i <- seq.int(2L, n)
        j <- rep.int(1L, length(i))
    } else if (kind == "revreference") {
        j <- seq.int(2L, n)
        i <- rep.int(1L, length(j))
    } else if (kind == "sequential") {
        i <- seq.int(2L, n)
        j <- i - 1L
    } else if (kind %in% c("pairwise", "revpairwise")) {
        mat <- matrix(FALSE, n, n)
        idx <- if (kind == "pairwise") {
            which(lower.tri(mat), arr.ind = TRUE)
        } else {
            which(upper.tri(mat), arr.ind = TRUE)
        }
        i <- idx[, "row"]
        j <- idx[, "col"]
    }

    list(i = i, j = j)
}


rowplan_hypothesis_binary_draws <- function(draws, i, j, lhs) {
    if (lhs == "difference") {
        out <- draws[i, , drop = FALSE] - draws[j, , drop = FALSE]
    } else if (lhs == "ratio") {
        out <- draws[i, , drop = FALSE] / draws[j, , drop = FALSE]
    } else {
        return(NULL)
    }

    return(out)
}


rowplan_hypothesis_trt_vs_ctrl_draws <- function(draws, lhs) {
    idx <- seq.int(2L, nrow(draws))
    ctrl <- matrix(draws[1L, ], nrow = length(idx), ncol = ncol(draws), byrow = TRUE)

    if (lhs == "difference") {
        out <- matrix(colMeans(draws[idx, , drop = FALSE] - ctrl), nrow = 1)
    } else if (lhs == "ratio") {
        out <- matrix(colMeans(draws[idx, , drop = FALSE] / ctrl), nrow = 1)
    } else {
        return(NULL)
    }

    return(out)
}


rowplan_hypothesis_meandev_draws <- function(draws, lhs) {
    center <- matrix(
        colMeans(draws),
        nrow = nrow(draws),
        ncol = ncol(draws),
        byrow = TRUE
    )

    if (lhs == "difference") {
        out <- draws - center
    } else if (lhs == "ratio") {
        out <- draws / center
    } else {
        return(NULL)
    }

    return(out)
}


rowplan_hypothesis_meanotherdev_draws <- function(draws, lhs) {
    center <- (
        matrix(
            colSums(draws),
            nrow = nrow(draws),
            ncol = ncol(draws),
            byrow = TRUE
        ) -
            draws
    ) / (nrow(draws) - 1L)

    if (lhs == "difference") {
        out <- draws - center
    } else if (lhs == "ratio") {
        out <- draws / center
    } else {
        return(NULL)
    }

    return(out)
}


rowplan_hypothesis_poly_draws <- function(draws, lhs) {
    w <- stats::contr.poly(nrow(draws))
    w <- w[, seq_len(min(5, ncol(w))), drop = FALSE]
    crossprod(w, draws)
}


rowplan_hypothesis_helmert_draws <- function(draws, lhs) {
    w <- stats::contr.helmert(nrow(draws))
    crossprod(w, draws)
}
