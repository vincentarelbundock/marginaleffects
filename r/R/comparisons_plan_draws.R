# Comparison stage of the plan applied to posterior draws.
#
# Models with posterior draws (brms, rstanarm, dbarts, MCMCglmm, ...) use the
# very same plan as point estimates: the comparison groups are recorded once
# by comparison_plan_record_groups() and then evaluated on n_pred x ndraws
# matrices of predictions. The helpers below evaluate one recorded group on
# such matrices, using closed forms of the built-in comparison functions where
# possible and falling back to one call per draw otherwise.

comparison_call_value <- function(call, hi, lo, y) {
    if (call$n == 0 || length(hi) == 0) {
        return(numeric(0))
    }

    value_args <- call$args
    value_args$hi <- hi
    value_args$lo <- lo
    if (isTRUE(call$uses_y)) {
        value_args$y <- y
    }
    con <- try(do_call(call$fun, value_args), silent = TRUE)
    comparison_validate_result(con, call$n)
    con
}


comparison_fast_path_vector_key <- function(call) {
    isTRUE(call$fun_key %in% c("difference", "ratio", "lnratio", "lift", "dydx", "expdydx"))
}


# Closed forms of the built-in comparisons, applied to every draw (column) at
# once. They mirror `comparison_function_dict` exactly.
comparison_fast_path_vector <- function(call, hi, lo) {
    if (!comparison_fast_path_vector_key(call)) return(NULL)
    eps <- call$args$eps
    if (call$fun_key %in% c("dydx", "expdydx") && !length(eps) %in% c(1L, nrow(hi))) {
        return(NULL)
    }
    out <- switch(
        call$fun_key,
        "difference" = hi - lo,
        "ratio" = hi / lo,
        "lnratio" = log(hi / lo),
        "lift" = (hi - lo) / lo,
        "dydx" = (hi - lo) / eps,
        "expdydx" = (exp(hi) - exp(lo)) / eps
    )
    if (anyNA(out)) {
        idx <- which(colSums(is.na(out)) > 0L)[[1]]
        comparison_validate_result(out[, idx], call$n)
    }
    out
}


comparison_fast_path_scalar <- function(call, hi, lo) {
    keys <- c("differenceavg", "ratioavg", "lnratioavg", "liftavg", "dydxavg")
    if (!isTRUE(call$fun_key %in% keys)) {
        return(NULL)
    }
    eps <- call$args$eps
    if (identical(call$fun_key, "dydxavg") && !length(eps) %in% c(1L, nrow(hi))) {
        return(NULL)
    }
    out <- switch(
        call$fun_key,
        "differenceavg" = colMeans(hi - lo),
        "ratioavg" = colMeans(hi) / colMeans(lo),
        "lnratioavg" = log(colMeans(hi) / colMeans(lo)),
        "liftavg" = colMeans(hi - lo) / colMeans(lo),
        "dydxavg" = colMeans((hi - lo) / eps)
    )
    if (anyNA(out)) {
        comparison_validate_result(out[[which(is.na(out))[[1]]]], call$n)
    }
    out
}


# Matrix version of comparison_plan_group_eval(): `hi`, `lo`, and `y` are
# matrices with one column per draw. Built-in comparisons with a closed form
# are applied to every column at once; anything else is applied column by
# column, validating each draw like the point-estimate path.
comparison_plan_group_eval_draws <- function(g, hi, lo, y) {
    hi <- hi[g$idx, , drop = FALSE]
    lo <- lo[g$idx, , drop = FALSE]
    if (isTRUE(g$uses_y)) {
        y <- y[g$idx, , drop = FALSE]
    }
    n_out <- length(g$out_idx)
    call <- list(
        fun = g$fun,
        fun_key = g$fun_key,
        args = g$args,
        uses_y = g$uses_y,
        n = g$n %||% length(g$idx)
    )

    fast <- NULL
    if (n_out == call$n) {
        fast <- comparison_fast_path_vector(call, hi, lo)
    }
    if (is.null(fast) && n_out == 1L) {
        fast <- comparison_fast_path_scalar(call, hi, lo)
    }
    if (!is.null(fast)) {
        return(matrix(fast, nrow = n_out))
    }

    cols <- lapply(seq_len(ncol(hi)), function(i) {
        con <- comparison_call_value(
            call = call,
            hi = hi[, i],
            lo = lo[, i],
            y = if (isTRUE(call$uses_y)) y[, i] else NULL
        )
        if (length(con) != n_out) {
            stop_sprintf("Internal error: comparison plan group changed shape.")
        }
        con
    })
    matrix(unlist(cols, use.names = FALSE), nrow = n_out)
}
