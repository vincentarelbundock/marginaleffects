prediction_prepare_newdata <- function(mfx, variables = NULL) {
    # analogous to comparisons(variables=list(...))
    if (!is.null(variables)) {
        mfx <- add_variables(
            variables = variables,
            mfx = mfx
        )
        args <- list(
            model = mfx@model,
            newdata = mfx@newdata,
            grid_type = "counterfactual",
            marginaleffects_internal = mfx
        )
        for (v in mfx@variables) {
            args[[v$name]] <- v$value
        }
        mfx@newdata <- do.call("datagrid", args)
    }

    if (!"rowid" %in% colnames(mfx@newdata)) {
        mfx@newdata[["rowid"]] <- seq_len(nrow(mfx@newdata))
    }

    mfx
}


prediction_plan_build <- function(
    mfx,
    type,
    model_perturbed = NULL,
    by = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    ...) {
    dots <- list(...)
    newdata <- mfx@newdata
    model <- if (is.null(model_perturbed)) mfx@model else model_perturbed

    out <- get_predict_error(
        model,
        newdata = newdata,
        type = type,
        mfx = mfx,
        ...
    )
    data.table::setDT(out)

    if (
        !"rowid" %in% colnames(out) &&
            "rowid" %in% colnames(newdata) &&
            nrow(out) == nrow(newdata)
    ) {
        out$rowid <- newdata$rowid
    }

    draws <- attr(out, "posterior_draws")
    raw_estimate <- out[["estimate"]]

    keep <- NULL
    if ("rowid" %in% colnames(out)) {
        idx_keep <- out$rowid > 0
        if (!all(idx_keep)) {
            keep <- which(idx_keep)
        }
    }

    tmp <- unpad(out, draws)
    out <- tmp$out
    draws <- tmp$draws

    payload <- NULL
    if (!isTRUE(checkmate::check_function(hypothesis))) {
        payload <- c("rowidcf", "marginaleffects_wts_internal")
        if (isTRUE(checkmate::check_character(by))) {
            payload <- c(payload, by)
        } else if (isTRUE(checkmate::check_data_frame(by))) {
            payload <- c(payload, setdiff(intersect(colnames(newdata), colnames(by)), "by"))
        }
        if (isTRUE(checkmate::check_formula(hypothesis))) {
            form <- sanitize_hypothesis_formula(hypothesis)
            payload <- c(payload, form$group, mfx@variable_names_datagrid)
        }
        payload <- unique(payload)
    }

    out <- merge_original_data(
        out,
        newdata,
        payload = payload,
        unit_level_only = FALSE
    )

    by <- sanitize_by(mfx, by, out = out, implicit = "group")

    if (is.null(draws)) {
        prediction_plan_build_frequentist(
            out = out,
            raw_estimate = raw_estimate,
            keep = keep,
            newdata = newdata,
            type = type,
            mfx = mfx,
            dots = dots,
            by = by,
            hypothesis = hypothesis,
            verbose = verbose,
            ...
        )
    } else {
        prediction_plan_build_bayesian(
            out = out,
            draws = draws,
            newdata = newdata,
            by = by,
            hypothesis = hypothesis,
            verbose = verbose,
            mfx = mfx,
            ...
        )
    }
}
