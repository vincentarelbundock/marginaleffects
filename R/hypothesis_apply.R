hypothesis_functions <- list(
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


hypothesis_apply <- function(x,
                             labels,
                             hypothesis_by = NULL,
                             fun_comparison,
                             fun_label,
                             newdata,
                             arbitrary = FALSE) {
    insight::check_if_installed("collapse")
    draws <- attr(x, "posterior_draws")
    args <- list(matrix(x$estimate), FUN = fun_comparison)

    if (is.null(labels)) {
        labels <- paste("Row", seq_len(nrow(x)))
    }

    if (is.null(hypothesis_by)) {
        applyfun <- collapse::dapply
        args[["drop"]] <- FALSE
        byval <- NULL
    } else {
        if (any(!hypothesis_by %in% c(colnames(x), colnames(newdata)))) {
            msg <- "Some `~ | groupid` variables were not found in `newdata`."
            stop(msg, call. = FALSE)
        }
        byval <- list(
            x[, intersect(hypothesis_by, colnames(x)), drop = FALSE],
            newdata[, intersect(hypothesis_by, colnames(newdata)), drop = FALSE]
        )
        byval <- do.call(cbind, Filter(is.data.frame, byval))
    }

    combined <- cbind(x[, "estimate"], byval)
    estimates <- combined[, lapply(.SD, fun_comparison), by = byval]

    lab <- function(x) names(fun_comparison(x))
    lab <- tryCatch(combined[, lapply(.SD, lab), by = byval], error = function(e) NULL)
    if (inherits(lab, "data.frame") && nrow(lab) == nrow(estimates)) {
        data.table::setnames(lab, old = "estimate", "hypothesis")
        cols <- setdiff(colnames(lab), colnames(estimates))
        estimates <- cbind(lab[, ..cols], estimates)
    }

    if (!is.null(labels) && !inherits(lab, "data.frame") || nrow(lab) == 0) {
        combined[, estimate := labels]
        labels <- combined[, lapply(.SD, fun_label), by = byval]
        data.table::setnames(labels, old = "estimate", "hypothesis")
        estimates <- cbind(labels, estimates)
    }

    out <- estimates

    if (!is.null(draws)) {
        draws <- data.table::data.table(draws)
        draws <- cbind(byval, draws)
        data.table::setDT(draws)
        draws <- draws[, lapply(.SD, fun_comparison), by = byval]
        cols <- setdiff(colnames(draws), colnames(byval))
        draws <- draws[, ..cols]
        draws <- as.matrix(draws)
        dimnames(draws) <- NULL
        if ("hypothesis" %in% colnames(out)) {
            row.names(draws) <- out$hypothesis
        }
    }

    attr(out, "posterior_draws") <- draws

    return(out)
}
