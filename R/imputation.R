process_imputation <- function(mfx) {
    insight::check_if_installed("mice")

    model <- mfx@model
    mfxcall <- mfx@call

    micedata <- tryCatch(get_modeldata_mids(model), error = function(e) NULL)

    # issue #1269: transforms must be applied after pooling
    if ("transform" %in% names(mfxcall)) {
        tr <- eval.parent(mfxcall[["transform"]])
        mfxcall[["transform"]] <- NULL
    } else {
        tr <- NULL
    }

    if (inherits(model, "mira")) {
        modellist <- model$analyses
    } else if (inherits(model, "amest")) {
        # amest already in the right format
        modellist <- model
    } else {
        stop_sprintf("MI class not implemented yet.")
    }

    mfxlist <- vector("list", length(modellist))
    for (i in seq_along(modellist)) {
        calltmp <- mfxcall
        calltmp[["model"]] <- modellist[[i]]

        # Handle deferred newdata processing for calls like subset(treat == 1)
        if ("newdata" %in% names(calltmp) && rlang::is_call(calltmp[["newdata"]])) {
            newdata_call <- calltmp[["newdata"]]
            if (rlang::call_name(newdata_call) == "subset") {
                # Add the model data as the 'x' argument for subset
                if (!"x" %in% rlang::call_args_names(newdata_call)) {
                    newdata_call <- rlang::call_modify(newdata_call, x = micedata[[i]])
                }
            } else if (rlang::call_name(newdata_call) == "filter") {
                # Add the model data as the '.data' argument for filter
                if (!".data" %in% rlang::call_args_names(newdata_call)) {
                    newdata_call <- rlang::call_modify(newdata_call, .data = micedata[[i]])
                }
            }
            # Evaluate the newdata call with the individual model's data
            calltmp[["newdata"]] <- eval(newdata_call)
        }

        mfxlist[[i]] <- eval.parent(calltmp)

        # save the S4 structure of a single model to host the eventual output
        if (i == 1) {
            out <- mfxlist[[i]]
        }

        # needed for mice::pool(); after saving `out`
        mfxlist[[i]]$term <- seq_len(nrow(mfxlist[[i]]))

        # Needed for mice::pool() to get dfcom, even when lean = TRUE
        attr(mfxlist[[i]], "marginaleffects")@model <- modellist[[i]]
        class(mfxlist[[i]]) <- c("marginaleffects_mids", class(mfxlist[[i]]))
    }

    mipool <- mice::pool(mfxlist)

    ti <- mice::tidy(mipool)
    for (col in c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high", "df")) {
        if (col %in% colnames(ti)) {
            out[[col]] <- ti[[col]]
        } else {
            out[[col]] <- NULL
        }
    }

    # Extract confidence level with fallback
    conf_level <- mfxcall[["conf_level"]]
    if (is.null(conf_level)) {
        conf_level <- 0.95 # Default fallback
    }

    out$df <- mipool$pooled$df

    # TODO: change this once we store the metadata in its final place
    out <- get_ci(out, attr(mfxlist[[1]], "marginaleffects"))

    out <- backtransform(out, sanitize_transform(tr))

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
