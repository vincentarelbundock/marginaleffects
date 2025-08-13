process_imputation <- function(x, call_attr, marginal_means = FALSE) {
    insight::check_if_installed("mice")

    # issue #1269: transforms must be applied after pooling
    if ("transform" %in% names(call_attr)) {
        tr <- eval.parent(call_attr[["transform"]])
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
            modeldata_i <- get_modeldata(
                x[[i]],
                additional_variables = FALSE
            )
            calltmp[["modeldata"]] <- modeldata_i

            # Handle deferred newdata processing for calls like subset(treat == 1)
            if ("newdata" %in% names(calltmp) && rlang::is_call(calltmp[["newdata"]])) {
                newdata_call <- calltmp[["newdata"]]
                if (rlang::call_name(newdata_call) == "subset") {
                    # Add the model data as the 'x' argument for subset
                    if (!"x" %in% rlang::call_args_names(newdata_call)) {
                        newdata_call <- rlang::call_modify(newdata_call, x = modeldata_i)
                    }
                } else if (rlang::call_name(newdata_call) == "filter") {
                    # Add the model data as the '.data' argument for filter
                    if (!".data" %in% rlang::call_args_names(newdata_call)) {
                        newdata_call <- rlang::call_modify(newdata_call, .data = modeldata_i)
                    }
                }
                # Evaluate the newdata call with the individual model's data
                calltmp[["newdata"]] <- eval(newdata_call)
            }
        }

        mfx_list[[i]] <- eval.parent(calltmp)
        if (i == 1) {
            out <- mfx_list[[1]]
        }
        mfx_list[[i]]$term <- seq_len(nrow(mfx_list[[i]]))

        # Needed for mice::pool() to get dfcom, even when lean = TRUE
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

    # Extract confidence level with fallback
    conf_level <- call_attr[["conf_level"]]
    if (is.null(conf_level)) {
        conf_level <- 0.95 # Default fallback
    }

    out <- get_ci_internal(out, conf_level = conf_level, df = mipool$pooled$df)

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
        stop_sprintf(
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
