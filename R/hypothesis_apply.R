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
        if (hypothesis_by %in% colnames(x)) {
            byval <- x[[hypothesis_by]]
        } else if (hypothesis_by %in% colnames(newdata)) {
            byval <- newdata[[hypothesis_by]]
        } else {
            msg <- sprintf("`%s` is not available in `newdata`.", hypothesis_by)
            stop(msg, call. = FALSE)
        }
        applyfun <- collapse::BY
        args[["g"]] <- byval
    }

    args[["FUN"]] <- fun_comparison
    estimates <- do.call(applyfun, args)

    if (!is.null(draws)) {
        args[[1]] <- draws
        draws <- do.call(applyfun, args)
    }

    if (arbitrary) {
        labels <- sub("^[^\\.]*\\.", "", row.names(estimates))
        if (is.null(labels)) {
            labels <- paste0("b", seq_along(estimates))
        }
    } else {
        args[["FUN"]] <- fun_label
        args[[1]] <- matrix(labels)
        labels <- do.call(applyfun, args)
    }

    if (!is.null(hypothesis_by)) {
        args[["FUN"]] <- function(x) x[1]
        args[[1]] <- byval
        byval <- do.call(applyfun, args)
    }

    out <- data.frame(
        estimate = as.vector(estimates),
        hypothesis = as.vector(labels)
    )

    if (!is.null(hypothesis_by) && !is.null(byval)) {
        out[[hypothesis_by]] <- byval
    }

    attr(out, "posterior_draws") <- draws

    return(out)
}
