# Builds the comparison/label pair for `pairwise` and `revpairwise`
# hypotheses. `pairwise` keeps the lower triangle of `outer(x, x, op)` and
# guards on the number of inputs; `revpairwise` keeps the upper triangle and
# guards on the number of pairs.
hypothesis_formula_pairwise <- function(op, reverse) {
    drop_tri <- if (reverse) lower.tri else upper.tri
    keep <- function(out) {
        diag(out) <- NA
        out[drop_tri(out)] <- NA
        out <- as.vector(out)
        out[!is.na(out)]
    }
    list(
        comparison = function(x) {
            safe_mode <- getOption("marginaleffects_safe", default = TRUE)
            n <- if (reverse) length(x) * (length(x) - 1) / 2 else length(x)
            if (n > 25 && isTRUE(safe_mode)) {
                msg <- "This command will generate many estimates. Set `options(marginaleffects_safe=FALSE)` to circumvent this guardrail."
                stop(msg, call. = FALSE)
            }
            keep(outer(x, x, op))
        },
        label = function(x) {
            x <- sprintf("(%s)", x)
            keep(outer(x, x, paste, sep = paste0(" ", op, " ")))
        }
    )
}


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
        ratio = hypothesis_formula_pairwise("/", reverse = FALSE),
        difference = hypothesis_formula_pairwise("-", reverse = FALSE)
    ),
    revpairwise = list(
        ratio = hypothesis_formula_pairwise("/", reverse = TRUE),
        difference = hypothesis_formula_pairwise("-", reverse = TRUE)
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
hypothesis_formula_groups <- function(x, newdata, group) {
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

    return(groupval)
}
