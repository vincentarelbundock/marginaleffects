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

    x <- data.table::as.data.table(data.table::copy(cmp_skeleton))
    groupval <- hypothesis_formula_groups(x, newdata, form$group)

    if (is.null(groupval)) {
        groups <- list(seq_len(nrow(x)))
    } else {
        combined <- data.table::data.table(marginaleffects_formula_idx = seq_len(nrow(x)))
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

    hyp <- list(kind = "formula", apply = apply)
    attr(hyp, "hypothesis_function_by") <- attr(cmp, "hypothesis_function_by")
    list(cmp = cmp, hyp = hyp)
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
    hyp <- list(kind = "matrix", apply = apply)
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

    parsed <- vector("list", length(hyps))
    rowlabels <- vector("list", length(hyps))
    for (i in seq_along(hyps)) {
        hyp <- hyps[[i]]
        if (
            isTRUE(grepl("\\bb\\d+\\b", hyp)) &&
                !any(grepl("\\bb\\d+\\b", cmp_skeleton[["term"]]))
        ) {
            for (j in seq_len(nrow(cmp_skeleton))) {
                tmp <- paste0("marginaleffects__", j)
                hyp <- gsub(paste0("b", j), tmp, hyp)
            }
            rowlabels[[i]] <- paste0("marginaleffects__", seq_len(nrow(cmp_skeleton)))
        } else {
            rowlabels[[i]] <- cmp_skeleton$term
        }
        parsed[[i]] <- parse(text = hyp)
    }

    apply_one <- function(est, expr, labels) {
        env <- new.env(parent = eval_parent)
        for (j in seq_along(est)) {
            assign(labels[[j]], est[[j]], envir = env)
        }
        eval(expr, envir = env)
    }
    apply <- function(est) {
        unlist(lapply(seq_along(parsed), function(i) {
            apply_one(est, parsed[[i]], rowlabels[[i]])
        }), use.names = FALSE)
    }

    hyp <- list(kind = "string", apply = apply)
    list(cmp = cmp, hyp = hyp)
}
