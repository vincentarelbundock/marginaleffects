hypothesis_formula_list <- list(
    reference = list(
        ratio = list(
            comparison = function(x) (x / x[1])[2:length(x)],
            label = function(x) sprintf("%s / %s", x, x[1])[2:length(x)]
        ),
        difference = list(
            comparison = function(x) (x - x[1])[2:length(x)],
            label = function(x) sprintf("%s - %s", x, x[1])[2:length(x)]
        )
    ),
    sequential = list(
        ratio = list(
            comparison = function(x) (x / data.table::shift(x))[2:length(x)],
            label = function(x) sprintf("%s / %s", x, data.table::shift(x))[2:length(x)]
        ),
        difference = list(
            comparison = function(x) (x - data.table::shift(x))[2:length(x)],
            label = function(x) sprintf("%s - %s", x, data.table::shift(x))[2:length(x)]
        )
    ),
    pairwise = list(
        ratio = list(
            comparison = function(x) {
                out <- outer(x, x, "/")
                if (ncol(out) > 25 && isFALSE(getOption("marginaleffects_safe", default = TRUE))) {
                    msg <- "This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail."
                    stop(msg, call. = FALSE)
                }
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            },
            label = function(x) {
                out <- outer(x, x, paste, sep = " / ")
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            }),
        difference = list(
            comparison = function(x) {
                out <- outer(x, x, "-")
                if (ncol(out) > 25 && isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
                    msg <- "This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail."
                    stop(msg, call. = FALSE)
                }
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            },
            label = function(x) {
                out <- outer(x, x, paste, sep = " - ")
                diag(out) <- NA
                out <- as.vector(out)
                out[!is.na(out)]
            })
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
            label = function(x) sprintf("%s / %s", x, "Mean")
        ),
        difference = list(
            comparison = function(x) x - mean(x),
            label = function(x) sprintf("%s - %s", x, "Mean")
        )
    ),
    meanotherdev = list(
        ratio = list(
            comparison = function(x) {
                s <- sum(x)
                m_other <- (s - x) / (length(x) - 1)
                x / m_other
            },
            label = function(x) sprintf("%s / %s", x, "Mean (other)")
        ),
        difference = list(
            comparison = function(x) {
                s <- sum(x)
                m_other <- (s - x) / (length(x) - 1)
                x - m_other
            },
            label = function(x) sprintf("%s - %s", x, "Mean (other)")
        )
    ),
    poly = list(
        dotproduct = list(
            comparison = function(x) {
                nx <- length(x)
                w <- stats::contr.poly(nx)
                w <- w[, 1:min(5, ncol(w))]
                as.vector(crossprod(w, matrix(x)))
            },
            label = function(x) {
                c("Linear", "Quadratic", "Cubic", "Quartic", "Quintic")[1:min(5, (length(x) - 1))]
            })
    )
)


#' @export
#' @keywords internal
hypothesis_formula <- function(x, hypothesis, newdata, by) {
    insight::check_if_installed("collapse")
    # default values
    draws <- attr(x, "posterior_draws")

    if (inherits(x, "data.frame")) {
        data.table::setDT(x)
    }
    if (inherits(newdata, "data.frame")) {
        data.table::setDT(newdata)
        if (nrow(newdata) != nrow(x)) {
            newdata <- NULL
        }
    }

    labels <- get_hypothesis_row_labels(x, by = by)

    form <- sanitize_hypothesis_formula(hypothesis)

    group <- form$group

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
        if (any(!group %in% c(colnames(x), colnames(newdata)))) {
            msg <- "Some `~ | groupid` variables were not found in `newdata`."
            stop(msg, call. = FALSE)
        }
        col_x <- intersect(group, colnames(x))
        col_newdata <- intersect(group, colnames(newdata))
        groupval <- list()
        if (length(col_x) > 0) {
            groupval <- c(groupval, list(x[, ..col_x, drop = FALSE]))
        }
        if (length(col_newdata) > 0) {
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
    lab <- tryCatch(combined[, lapply(.SD, lab), keyby = groupval], error = function(e) NULL)
    if (inherits(lab, "data.frame") && nrow(lab) == nrow(estimates)) {
        data.table::setnames(lab, old = "estimate", "hypothesis")
        cols <- setdiff(colnames(lab), colnames(estimates))
        estimates <- cbind(lab[, ..cols], estimates)
    }

    if (!is.null(labels) && !inherits(lab, "data.frame") || nrow(lab) == 0) {
        combined[, estimate := labels]
        labels <- tryCatch(combined[, lapply(.SD, fun_label), keyby = groupval],
            error = function(e) NULL)
        if (inherits(labels, "data.frame") && nrow(labels) == nrow(estimates)) {
            data.table::setnames(labels, old = "estimate", "hypothesis")
            estimates <- cbind(labels, estimates)
        }
    }

    out <- estimates

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
