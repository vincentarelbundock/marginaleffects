get_unique_index <- function(x, term_only = FALSE) {
    idx <- c("term", "contrast", grep("^contrast_", colnames(x), value = TRUE))

    if (!term_only) {
        by <- attr(x, "by")
        if (isTRUE(checkmate::check_data_frame(by))) {
            idx <- c(idx, colnames(by))
        } else {
            idx <- c(idx, by)
        }
        explicit <- attr(attr(x, "newdata"), "explicit")
        if (isTRUE(checkmate::check_character(explicit))) {
            idx <- explicit
        }
    }

    idx <- intersect(unique(idx), colnames(x))

    if (length(idx) == 0) {
        return(NULL)
    } else if (length(idx) == 1) {
        return(x[[idx]])
    }

    out <- x[, idx, drop = FALSE]

    for (i in ncol(out):2) {
        if (length(unique(out[[i]])) == 1) {
            out[[i]] <- NULL
        }
    }

    out <- apply(out, 1, paste, collapse = ", ")
    return(out)
}


get_marginaleffects_attributes <- function(x, exclude = NULL, include = NULL, include_regex = NULL) {
    out <- list()
    attr_names <- names(attributes(x))
    attr_names <- setdiff(attr_names, exclude)
    if (!is.null(include)) attr_names <- intersect(attr_names, include)
    if (!is.null(include_regex)) attr_names <- attr_names[grepl(include_regex, attr_names)]
    for (n in attr_names) {
        out[[n]] <- attr(x, n)
    }
    return(out)
}

set_marginaleffects_attributes <- function(x, attr_cache, prefix = "") {
    for (n in names(attr_cache)) {
        attr(x, paste0(prefix, n)) <- attr_cache[[n]]
    }
    return(x)
}


warn_once <- function(msg, id) {
    msg <- c(msg, "", "This warning appears once per session.")
    if (isTRUE(getOption(id, default = TRUE))) {
        # insight::format_warning(msg, call. = FALSE)
        warning(msg, call. = FALSE)
        opts <- list(FALSE)
        names(opts) <- id
        options(opts)
    }
}


# Cross join a list of data tables
# Source: https://github.com/Rdatatable/data.table/issues/1717#issuecomment-545758165
cjdt <- function(dtlist) {
    Reduce(function(DT1, DT2) cbind(DT1, DT2[rep(1:.N, each = nrow(DT1))]), dtlist)
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
    if (is.null(out) && !is.null(msg)) stop(msg)
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
            error = function(e) x)
    } else {
        out <- x
    }
    return(out)
}

# faster than all(x %in% 0:1)
is_binary <- function(x) {
    isTRUE(checkmate::check_integerish(
        x, null.ok = TRUE, upper = 1, lower = 0, any.missing = FALSE)
    )
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
    stop("set_coef() substitution error. Please report on Github with a reproducible example: https://github.com/vincentarelbundock/marginaleffects/issues", call. = FALSE)
  }

  return(x)
}


group_to_factor <- function(group, model) {
    dv <- try(insight::get_response(model), silent = TRUE)
    if (inherits(dv, "factor")) {
        group <- factor(group, levels(dv))
    }
    return(group)
}
