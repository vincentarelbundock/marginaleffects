process_imputation <- function(x, call_attr, marginal_means = FALSE) {
    insight::check_if_installed("mice")

    # issue #1269: transforms must be applied after pooling
    if ("transform" %in% names(call_attr)) {
        tr <- evalup(call_attr[["transform"]])
        call_attr[["transform"]] <- NULL
    } else {
        tr <- NULL
    }

    if (inherits(x, "mira")) {
        x <- x$analyses
    } else if (inherits(x, "amest")) {
        x <- x
    }

    mfx_list <- vector("list", length(x))
    for (i in seq_along(x)) {
        calltmp <- call_attr
        calltmp[["model"]] <- x[[i]]

        # not sure why but this breaks marginal_means on "modeldata specified twice"
        if (isFALSE(marginal_means)) {
            calltmp[["modeldata"]] <- get_modeldata(
                x[[i]],
                additional_variables = FALSE
            )
        }

        mfx_list[[i]] <- evalup(calltmp)
        if (i == 1) {
            out <- mfx_list[[1]]
        }
        mfx_list[[i]]$term <- seq_len(nrow(mfx_list[[i]]))

        #Needed for mice::pool() to get dfcom, even when lean = TRUE
        attr(mfx_list[[i]], "model") <- x[[i]]

        class(mfx_list[[i]]) <- c("marginaleffects_mids", class(mfx_list[[i]]))
    }
    mipool <- mice::pool(mfx_list)
    for (col in c("estimate", "statistic", "p.value", "conf.low", "conf.high")) {
        if (col %in% colnames(out) && col %in% colnames(mipool$pooled)) {
            out[[col]] <- mipool$pooled[[col]]
        } else {
            out[[col]] <- NULL
        }
    }
    if ("df" %in% colnames(mipool$pooled)) {
        out$df <- mipool$pooled$df
    }
    out$std.error <- sqrt(mipool$pooled$t)

    out <- get_ci(
        out,
        vcov = NULL,
        conf_level = call_attr[["conf_level"]],
        df = mipool$pooled$df
    )

    out <- backtransform(out, sanitize_transform(tr))

    # Global option for lean return object
    lean <- getOption("marginaleffects_lean", default = FALSE)

    if (!lean) {
        attr(out, "inferences") <- mipool
        attr(out, "model") <- mice::pool(lapply(mfx_list, attr, "model"))
    }

    return(out)
}

#' tidy helper
#'
#' @noRd
#' @export
tidy.marginaleffects_mids <- function(x, ...) {
    if (!"std.error" %in% colnames(x)) {
        insight::format_error(
            'The output does not include a `std.error` column. Some models do not generate standard errors when estimates are backtransformed (e.g., GLM models). One solution is to use `type="response"` for those models.'
        )
    }
    out <- as.data.frame(x[, c("estimate", "std.error")])
    out$term <- seq_len(nrow(out))
    return(out)
}


#' glance helper
#'
#' @noRd
#' @export
glance.marginaleffects_mids <- function(x, ...) {
    data.frame()
}
