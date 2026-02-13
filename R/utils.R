get_unique_index <- function(x, term_only = FALSE) {
    idx <- c("term", "contrast", grep("^contrast_", colnames(x), value = TRUE))
    idx <- intersect(unique(idx), colnames(x))
    if (length(idx) == 0) {
        return(NULL)
    }
    if (length(idx) == 1) {
        return(x[[idx]])
    }
    out <- x[, idx, drop = FALSE]
    for (i in ncol(out):2) {
        if (length(unique(out[[i]])) == 1) {
            out[[i]] <- NULL
        }
    }
    out <- apply(out, 1, toString)
    return(out)
}


warn_once <- function(msg, id) {
    if (!isTRUE(getOption(id, default = TRUE)) || !isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
        return(invisible())
    }
    msg <- paste(msg, "This warning appears once per session.")
    # warn_sprintf(msg, call. = FALSE)
    warning(msg, call. = FALSE)
    opts <- list(FALSE)
    names(opts) <- id
    options(opts)
}


# Cross join a list of data tables
# Source: https://github.com/Rdatatable/data.table/issues/1717#issuecomment-545758165
cjdt <- function(dtlist) {
    Reduce(
        function(DT1, DT2) cbind(DT1, DT2[rep(seq_len(.N), each = nrow(DT1))]),
        dtlist
    )
}


# recurse up. mostly useful for `tinytest`
# this is dumb, but it's late and i don't feel like thinking about this
evalup <- function(xcall) {
    out <- myTryCatch(eval(xcall))
    if (inherits(out$error, "simpleError")) {
        msg <- out$error$message
        out <- NULL
    } else {
        msg <- NULL
        out <- out$value
    }
    for (i in 1:10) {
        if (is.null(out)) {
            out <- hush(eval(xcall, parent.frame(i)))
        }
    }
    if (is.null(out) && !is.null(msg)) {
        stop(msg)
    }
    return(out)
}


merge_by_rowid <- function(x, y) {
    # return data
    # very import to avoid sorting, otherwise bayesian draws won't fit predictions
    # merge only with rowid; not available for hypothesis
    mergein <- setdiff(colnames(y), colnames(x))
    if ("rowid" %in% colnames(x) && "rowid" %in% colnames(y) && length(mergein) > 0) {
        idx <- c("rowid", mergein)
        if (!data.table::is.data.table(y)) {
            data.table::setDT(y)
            tmp <- y[, ..idx]
        } else {
            tmp <- y[, ..idx]
        }
        # TODO: this breaks in mclogit. maybe there's a more robust merge
        # solution for weird grouped data. But it seems fine because
        # `predictions()` output does include the original predictors.
        out <- tryCatch(
            merge(x, tmp, by = "rowid", sort = FALSE),
            error = function(e) x
        )
    } else {
        out <- x
    }
    return(out)
}

# faster than all(x %in% 0:1)
is_binary <- function(x) {
    isTRUE(checkmate::check_integerish(
        x,
        null.ok = TRUE,
        upper = 1,
        lower = 0,
        any.missing = TRUE,
        all.missing = FALSE
    ))
}


sub_named_vector <- function(x, y) {
    # issue 1005
    xlab <- gsub("^`|`$", "", names(x))
    ylab <- gsub("^`|`$", "", names(y))

    idx <- match(ylab, xlab)
    if (length(stats::na.omit(idx)) > 0) {
        x[stats::na.omit(idx)] <- y[!is.na(idx)]
    } else if (length(y) == length(x)) {
        return(y)
    } else {
        stop(
            "set_coef() substitution error. Please report on Github with a reproducible example: https://github.com/vincentarelbundock/marginaleffects/issues",
            call. = FALSE
        )
    }

    return(x)
}


group_to_factor <- function(group, model) {
    dv <- try(insight::get_response(model), silent = TRUE)
    if (inherits(dv, "factor")) {
        if (length(unique(group)) == nlevels(dv)) {
            group <- factor(group, levels(dv))
        }
    }
    return(group)
}


...get <- function(x, ifnotfound = NULL) {
    eval(
        quote(
            if (!anyNA(.m1 <- match(.x, ...names())) && !is.null(.m2 <- ...elt(.m1))) {
                .m2
            } else {
                .ifnotfound
            }),
        pairlist(.x = x[1L], .ifnotfound = ifnotfound),
        parent.frame(1L)
    )
}


...mget <- function(x) {
    found <- match(x, eval(quote(...names()), parent.frame(1L)))
    not_found <- is.na(found)
    if (all(not_found)) {
        return(list())
    }
    stats::setNames(
        lapply(found[!not_found], function(z) {
            eval(
                quote(...elt(.z)),
                pairlist(.z = z),
                parent.frame(3L)
            )
        }),
        x[!not_found]
    )
}

stop_deprecate <- function(old, new = NULL) {
    if (is.null(new)) {
        msg <- sprintf("The `%s` argument is not supported.", old)
    } else {
        msg <- sprintf("The `%s` argument is not supported. Please use `%s` instead.", old, new)
    }
    stop(msg, call. = FALSE)
}


stop_sprintf <- function(msg, ...) {
    dots <- list(...)
    if (length(dots) > 0) {
        msg <- sprintf(msg, ...)
    }
    stop(msg, call. = FALSE)
}


warn_sprintf <- function(msg, ...) {
    if (!isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
        return(invisible())
    }
    dots <- list(...)
    if (length(dots) > 0) {
        msg <- sprintf(msg, ...)
    }
    warning(msg, call. = FALSE)
}


# Like do.call() but avoids inlining argument values into the call expression.
# When do.call(fn, list(huge_model, huge_data)) errors, R stores the entire
# inlined data in sys.calls(), and IDEs (RStudio/Positron) try to deparse it,
# causing multi-minute hangs. This builds a call with symbol references instead.
# See: https://github.com/vincentarelbundock/marginaleffects/issues/1663
do_call <- function(what, args) {
    call_env <- new.env(parent = parent.frame())
    call_env[[".what"]] <- what
    arg_names <- names(args) %||% rep("", length(args))
    call_list <- vector("list", length(args) + 1L)
    call_list[[1L]] <- as.symbol(".what")
    for (i in seq_along(args)) {
        sym_name <- sprintf(".arg%d", i)
        call_env[[sym_name]] <- args[[i]]
        call_list[[i + 1L]] <- as.symbol(sym_name)
    }
    names(call_list) <- c("", arg_names)
    eval(as.call(call_list), call_env)
}
