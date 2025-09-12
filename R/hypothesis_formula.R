hypothesis_formula_list <- list(
    reference = list(
        ratio = list(
            comparison = function(x) (x / x[1])[2:length(x)],
            label = function(x) sprintf("(%s) / (%s)", x, x[1])[2:length(x)]
        ),
        difference = list(
            comparison = function(x) (x - x[1])[2:length(x)],
            label = function(x) sprintf("(%s) - (%s)", x, x[1])[2:length(x)]
        )
    ),
    revreference = list(
        ratio = list(
            comparison = function(x) (x[1] / x)[2:length(x)],
            label = function(x) sprintf("(%s) / (%s)", x[1], x)[2:length(x)]
        ),
        difference = list(
            comparison = function(x) (x[1] - x)[2:length(x)],
            label = function(x) sprintf("(%s) - (%s)", x[1], x)[2:length(x)]
        )
    ),
    sequential = list(
        ratio = list(
            comparison = function(x) (x / data.table::shift(x))[2:length(x)],
            label = function(x) sprintf("(%s) / (%s)", x, data.table::shift(x))[2:length(x)]
        ),
        difference = list(
            comparison = function(x) (x - data.table::shift(x))[2:length(x)],
            label = function(x) sprintf("(%s) - (%s)", x, data.table::shift(x))[2:length(x)]
        )
    ),
    pairwise = list(
        ratio = list(
            comparison = function(x) {
                safe_mode <- getOption("marginaleffects_safe", default = TRUE)
                if (length(x) > 25 && isTRUE(safe_mode)) {
                    msg <- "This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail."
                    stop(msg, call. = FALSE)
                }
                out <- outer(x, x, "/")
                diag(out) <- NA
                out[upper.tri(out)] <- NA # Set lower triangle to NA
                out <- as.vector(out)
                out <- out[!is.na(out)] # Keep only non-NA values
                out
            },
            label = function(x) {
                x <- sprintf("(%s)", x)
                out <- outer(x, x, paste, sep = " / ")
                out[upper.tri(out)] <- NA # Set lower triangle to NA
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            }
        ),
        difference = list(
            comparison = function(x) {
                safe_mode <- getOption("marginaleffects_safe", default = TRUE)
                if (length(x) > 25 && isTRUE(safe_mode)) {
                    msg <- "This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail."
                    stop(msg, call. = FALSE)
                }
                out <- outer(x, x, "-")
                diag(out) <- NA
                out[upper.tri(out)] <- NA # Set lower triangle to NA
                out <- as.vector(out)
                out <- out[!is.na(out)] # Keep only non-NA values
                out
            },
            label = function(x) {
                x <- sprintf("(%s)", x)
                out <- outer(x, x, paste, sep = " - ")
                out[upper.tri(out)] <- NA # Set lower triangle to NA
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            }
        )
    ),
    revpairwise = list(
        ratio = list(
            comparison = function(x) {
                out <- outer(x, x, "/")
                diag(out) <- NA
                out[lower.tri(out)] <- NA # Set lower triangle to NA
                out <- as.vector(out)
                out <- out[!is.na(out)] # Keep only non-NA values
                safe_mode <- getOption("marginaleffects_safe", default = TRUE)
                if (length(out) > 25 && isTRUE(safe_mode)) {
                    msg <- "This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail."
                    stop(msg, call. = FALSE)
                }
                out
            },
            label = function(x) {
                x <- sprintf("(%s)", x)
                out <- outer(x, x, paste, sep = " / ")
                out[lower.tri(out)] <- NA # Set lower triangle to NA
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            }
        ),
        difference = list(
            comparison = function(x) {
                out <- outer(x, x, "-")
                diag(out) <- NA
                out[lower.tri(out)] <- NA # Set lower triangle to NA
                out <- as.vector(out)
                out <- out[!is.na(out)] # Keep only non-NA values
                safe_mode <- getOption("marginaleffects_safe", default = TRUE)
                if (length(out) > 25 && isTRUE(safe_mode)) {
                    msg <- "This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail."
                    stop(msg, call. = FALSE)
                }
                out
            },
            label = function(x) {
                x <- sprintf("(%s)", x)
                out <- outer(x, x, paste, sep = " - ")
                out[lower.tri(out)] <- NA # Set lower triangle to NA
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            }
        )
    ),
    trt_vs_ctrl = list(
        ratio = list(
            comparison = function(x) mean(x[2:length(x)] / x[1]),
            label = function(x) "Mean(Trt) / Ctrl"
        ),
        difference = list(
            comparison = function(x) mean(x[2:length(x)] - x[1]),
            label = function(x) "Mean(Trt) - Ctrl"
        )
    ),
    meandev = list(
        ratio = list(
            comparison = function(x) x / mean(x),
            label = function(x) sprintf("(%s) / %s", x, "Mean")
        ),
        difference = list(
            comparison = function(x) x - mean(x),
            label = function(x) sprintf("(%s) - %s", x, "Mean")
        )
    ),
    meanotherdev = list(
        ratio = list(
            comparison = function(x) {
                s <- sum(x)
                m_other <- (s - x) / (length(x) - 1)
                x / m_other
            },
            label = function(x) sprintf("(%s) / %s", x, "Mean (other)")
        ),
        difference = list(
            comparison = function(x) {
                s <- sum(x)
                m_other <- (s - x) / (length(x) - 1)
                x - m_other
            },
            label = function(x) sprintf("(%s) - %s", x, "Mean (other)")
        )
    ),
    poly = list(
        dotproduct = list(
            comparison = function(x) {
                nx <- length(x)
                w <- stats::contr.poly(nx)
                w <- w[, seq_len(min(5, ncol(w))), drop = FALSE]
                as.vector(crossprod(w, matrix(x)))
            },
            label = function(x) {
                c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic")[
                    1:min(5, (length(x) - 1))
                ]
            }
        )
    ),
    helmert = list(
        dotproduct = list(
            comparison = function(x) {
                nx <- length(x)
                w <- stats::contr.helmert(nx)
                out <- as.vector(x %*% w)
                names(out) <- paste("Helmert", seq_along(out))
                return(out)
            },
            label = function(x) x
        )
    )
)


#' Internal function
#'
#' @noRd
#' @keywords internal
hypothesis_formula <- function(x, hypothesis, newdata, by) {
    # default values
    draws <- attr(x, "posterior_draws")

    form <- sanitize_hypothesis_formula(hypothesis)

    group <- form$group

    x <- data.table::as.data.table(x)

    if (isTRUE(checkmate::check_character(by))) {
        bycols <- setdiff(by, group)
    } else {
        bycols <- by
    }
    labels <- get_labels(x, by = bycols, hypothesis_by = group)

    if (isTRUE(form$lhs == "arbitrary_function")) {
        fun_comparison <- sprintf("function(x) %s", form$rhs)
        fun_label <- sprintf("function(x) suppressWarnings(names(%s))", form$rhs)
        fun_comparison <- eval(parse(text = fun_comparison))
        fun_label <- eval(parse(text = fun_label))
    } else {
        fun_label <- hypothesis_formula_list[[form$rhs]][[form$lhs]]$label
        fun_comparison <- hypothesis_formula_list[[form$rhs]][[form$lhs]]$comparison
    }

    args <- list(matrix(x$estimate), FUN = fun_comparison)

    if (is.null(labels)) {
        labels <- paste("Row", seq_len(nrow(x)))
    }

    if (!is.null(group)) {
        if (!all(group %in% c(colnames(x), colnames(newdata)))) {
            msg <- "Some `~ | groupid` variables were not found in `newdata`."
            stop(msg, call. = FALSE)
        }
        col_x <- intersect(group, colnames(x))
        col_newdata <- intersect(group, colnames(newdata))
        groupval <- list()
        if (length(col_x) > 0) {
            groupval <- c(groupval, list(x[, ..col_x, drop = FALSE]))
        } else if (length(col_newdata) > 0) {
            groupval <- c(groupval, list(newdata[, ..col_newdata, drop = FALSE]))
        }
        groupval <- do.call(cbind, Filter(is.data.frame, groupval))
    } else {
        groupval <- NULL
    }

    combined <- list(x[, "estimate", drop = FALSE], groupval)
    combined <- Filter(function(x) inherits(x, "data.frame"), combined)
    combined <- do.call(cbind, combined)
    data.table::setDT(combined)

    if (is.null(groupval)) {
        estimates <- combined[, lapply(.SD, fun_comparison)]
    } else {
        estimates <- combined[, lapply(.SD, fun_comparison), keyby = groupval]
    }

    lab <- function(x) suppressWarnings(names(fun_comparison(x)))
    lab <- tryCatch(
        combined[, lapply(.SD, lab), keyby = groupval],
        error = function(e) NULL
    )

    if (inherits(lab, "data.frame") && nrow(lab) == nrow(estimates)) {
        data.table::setnames(lab, old = "estimate", "hypothesis")
        cols <- setdiff(colnames(lab), colnames(estimates))
        estimates <- cbind(lab[, ..cols], estimates)
    }

    if (!is.null(labels) && !inherits(lab, "data.frame") || nrow(lab) == 0) {
        combined[, estimate := labels]
        labels <- tryCatch(
            combined[, lapply(.SD, fun_label), keyby = groupval],
            error = function(e) NULL
        )
        if (inherits(labels, "data.frame") && nrow(labels) == nrow(estimates)) {
            data.table::setnames(labels, old = "estimate", "hypothesis")
            cols <- setdiff(colnames(estimates), colnames(labels))
            estimates <- cbind(labels, subset(estimates, select = cols))
        }
    }

    out <- estimates

    # Sometimes we get duplicated `term` columns
    # drop all instances after the first
    idx <- grep("^term$", colnames(out))
    if (length(idx) > 1) {
        idx <- idx[2:length(idx)]
        out <- out[, -..idx]
    }

    if (!is.null(draws)) {
        draws <- matrix_apply_column(draws, FUN = fun_comparison, by = groupval)
        if ("hypothesis" %in% colnames(out)) {
            row.names(draws) <- out$hypothesis
        }
    }

    attr(out, "posterior_draws") <- draws
    attr(out, "hypothesis_function_by") <- form$group

    return(out)
}
