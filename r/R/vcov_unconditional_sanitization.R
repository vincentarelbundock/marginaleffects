stop_unconditional <- function(
    vcov,
    reason) {

    if (!is_unconditional_vcov(vcov)) {
        return(invisible(TRUE))
    }

    msg <- switch(
        reason,
        "imputation" = paste0(
            "`vcov = \"unconditional\"` is not supported for ",
            "multiple-imputation model objects or pooled multiple-imputation results."
        ),
        "hypotheses" = paste0(
            "`vcov = \"unconditional\"` is only available for predictions, ",
            "comparisons, and slopes computed by `marginaleffects`. Use ",
            "`avg_predictions()`, `avg_comparisons()`, or `avg_slopes()` with ",
            "`vcov = \"unconditional\"`, then call `hypotheses()` on the result ",
            "if needed."
        ),
        "inferences" = paste0(
            "`inferences()` is not available for objects computed with ",
            "`vcov = \"unconditional\"`. Use the unconditional standard errors ",
            "from the original `avg_*()` call, or refit the original call with ",
            "`vcov = FALSE` before applying `inferences()`."
        )
    )
    if (is.null(msg)) {
        stop_sprintf("Internal error: unknown unconditional stop reason: %s.", reason %||% "NULL")
    }
    stop_sprintf(msg)
}


sanitize_unconditional_vcov_request <- function(vcov, mfx, df = "residual", df_supplied = FALSE) {
    if (!is_unconditional_vcov(vcov)) {
        return(vcov)
    }

    calling_function <- tryCatch(mfx@calling_function, error = function(e) NULL)
    validate_unconditional_model_support(mfx@model, kind = calling_function)

    if (inherits(vcov, "marginaleffects_vcov_unconditional")) {
        vcov_spec <- vcov$vcov
        df <- vcov$df
    } else {
        vcov_spec <- "HC1"
        if (isTRUE(df_supplied)) {
            fun <- if (is.null(calling_function)) "this function" else sprintf("`%s()`", calling_function)
            msg <- paste0(
                "The top-level `df` argument was supplied to ",
                sprintf("%s with `vcov = \"unconditional\"`. ", fun),
                "You can also set degrees of freedom directly with ",
                "`vcov = unconditional(df = ...)`."
            )
            warn_once(msg, "marginaleffects_unconditional_df_switch")
        }
    }

    modeldata <- data.table::as.data.table(mfx@modeldata)
    auxdata <- if (inherits(vcov_spec, "formula")) {
        get_unconditional_auxdata(mfx, nrow(modeldata))
    }
    vcov_info <- sanitize_unconditional_vcov_arg(vcov_spec, modeldata, auxdata)
    df_value <- sanitize_unconditional_df(df, mfx@model, vcov_info)

    structure(
        list(vcov = vcov_info, df = df_value),
        class = "marginaleffects_vcov_unconditional"
    )
}


validate_unconditional_model_support <- function(model, kind, type = NULL) {
    if (inherits(model, c("mira", "amest"))) {
        stop_unconditional("unconditional", "imputation")
    }

    # Deliberately inspect the primary class to reject unvalidated subclasses
    # such as `mlm`, even when they also inherit from a supported class.
    primary_class <- class(model)[1]
    if (isTRUE(primary_class %in% c("lm", "glm", "survreg", "tobit"))) {
        return(invisible(TRUE))
    }

    if (inherits(model, "fixest")) {
        has_fixed_effects <- !is.null(model[["fixef_vars"]])
        if (!has_fixed_effects) {
            return(invisible(TRUE))
        }
        if (identical(kind, "predictions")) {
            msg <- paste0(
                "`vcov = \"unconditional\"` is not supported for average ",
                "predictions from `fixest` models with fixed effects because ",
                "fixed-effect uncertainty is not represented."
            )
            stop_sprintf(msg)
        }
        if (!identical(model[["method_type"]], "feols")) {
            msg <- paste0(
                "`vcov = \"unconditional\"` is not supported for nonlinear ",
                "`fixest` models with fixed effects because fixed-effect ",
                "uncertainty is not represented."
            )
            stop_sprintf(msg)
        }
        return(invisible(TRUE))
    }

    if (inherits(model, "coxph")) {
        if (is.null(type) || isTRUE(type %in% c("lp", "risk"))) {
            return(invisible(TRUE))
        }
        if (isTRUE(type %in% c("survival", "expected"))) {
            msg <- paste0(
                "`vcov = \"unconditional\"` is not supported for `coxph` ",
                "predictions with `type = \"%s\"` because uncertainty in the ",
                "baseline hazard is not represented. Use `type = \"lp\"`, ",
                "`type = \"risk\"`, or a bootstrap method."
            )
            stop_sprintf(msg, type)
        }
    }

    reason <- paste0(
        "only explicitly validated model classes are supported. ",
        "Currently supported classes include `lm`, `glm`, selected ",
        "`fixest`, selected survival models, and `tobit` models"
    )
    msg <- paste0(
        "`vcov = \"unconditional\"` is not currently supported for models ",
        "of class \"%s\": %s. Use a bootstrap method instead."
    )
    stop_sprintf(
        msg,
        class(model)[1],
        reason
    )
}


validate_unconditional_plan_target <- function(plan) {
    scalar_comparison <- identical(plan$kind, "comparisons") &&
        any(vapply(plan$groups, function(group) {
            isTRUE(group$scalar) && length(group$idx) > 1L
        }, logical(1)))

    if (!is.null(plan$agg) || scalar_comparison) {
        return(invisible(TRUE))
    }

    if (!is.null(plan$hyp)) {
        msg <- paste0(
            "`vcov = \"unconditional\"` does not support `hypothesis` ",
            "applied directly to unit-level effects. Use an `avg_*()` ",
            "function, a `by` argument, or a scalar comparison before ",
            "`hypothesis`."
        )
        stop_sprintf(msg)
    }

    msg <- paste0(
        "`vcov = \"unconditional\"` is only supported for averaged or ",
        "aggregated effects. Use an `avg_*()` function, a `by` argument, ",
        "or a scalar comparison."
    )
    stop_sprintf(msg)
}


sanitize_unconditional_plan <- function(
    plan,
    modeldata,
    n,
    df,
    n_estimates,
    allow_mismatch = character()) {

    source <- if (identical(plan$kind, "predictions")) {
        plan$predict_args$newdata
    } else {
        plan$predict_args$original
    }
    if (identical(plan$kind, "predictions") && !is.null(plan$keep)) {
        source <- source[plan$keep, , drop = FALSE]
    }

    msg <- paste0(
        "`vcov = \"unconditional\"` requires effects evaluated over ",
        "original model-data rows, a subset of original rows with valid ",
        "row IDs, or a full counterfactual grid that preserves `rowidcf`."
    )
    rowid <- resolve_unconditional_rowid(
        source,
        n,
        modeldata = modeldata,
        allow_mismatch = allow_mismatch,
        message = msg
    )

    validate_unconditional_plan_target(plan)

    if (is.numeric(df) && !length(df) %in% c(1L, n_estimates)) {
        stop_sprintf(
            "The `df` argument must have length 1 or match the number of estimates (%d). Got length %d.",
            n_estimates,
            length(df)
        )
    }

    rowid
}


get_unconditional_allow_mismatch <- function(mfx, variables = NULL) {
    out <- character()

    if (isTRUE(checkmate::check_list(variables))) {
        for (v in variables) {
            if (isTRUE(checkmate::check_string(v$name))) {
                out <- c(out, v$name)
            }
        }
    }

    out <- c(out, tryCatch(mfx@variable_names_datagrid, error = function(e) character()))
    unique(stats::na.omit(out))
}


resolve_unconditional_rowid <- function(
    x,
    n,
    modeldata = NULL,
    allow_mismatch = character(),
    message = NULL) {

    if (is.null(message)) {
        message <- "`vcov = \"unconditional\"` currently requires evaluation rows to map to the original model data."
    }
    out <- NULL

    common <- character()
    if (!is.null(modeldata)) {
        ignored <- c(
            "rowid",
            "rowidcf",
            "marginaleffects_wts_internal",
            "rowid_dedup"
        )
        common <- intersect(colnames(x), colnames(modeldata))
        common <- setdiff(common, c(ignored, allow_mismatch))
    }

    rowid_cols <- intersect(c("rowidcf", "rowid"), colnames(x))
    for (rowid_col in rowid_cols) {
        candidate <- as.integer(x[[rowid_col]])
        if (anyNA(candidate) || !all(candidate %in% seq_len(n))) {
            next
        }
        if (identical(rowid_col, "rowidcf") &&
            length(allow_mismatch) == 0L &&
            !setequal(unique(candidate), seq_len(n))) {
            next
        }
        if (is.null(modeldata)) {
            out <- candidate
        } else {
            matches <- vapply(common, function(column) {
                isTRUE(all.equal(
                    x[[column]],
                    modeldata[[column]][candidate],
                    check.attributes = FALSE
                ))
            }, logical(1))
            if (all(matches)) {
                out <- candidate
            }
        }
        if (!is.null(out)) {
            break
        }
    }

    if (is.null(out) && !is.null(modeldata)) {
        matched <- match_unconditional_source_modeldata(
            x,
            modeldata,
            common = common)
        if (!is.null(matched)) {
            out <- matched
        }
    }

    if (is.null(out) || anyNA(out) || any(out < 1L) || any(out > n)) {
        stop_sprintf(message)
    }
    as.integer(out)
}


match_unconditional_source_modeldata <- function(source, modeldata, common) {
    source <- data.table::as.data.table(source)
    modeldata <- data.table::as.data.table(modeldata)
    if (length(common) == 0) {
        return(NULL)
    }

    model_keys <- data.table::copy(modeldata[, common, with = FALSE])
    if (any(duplicated(model_keys, by = common))) {
        return(NULL)
    }
    model_keys[, ".marginaleffects_unconditional_rowid" := seq_len(.N)]
    source_keys <- source[, common, with = FALSE]
    out <- model_keys[source_keys, on = common][[".marginaleffects_unconditional_rowid"]]
    if (length(out) != nrow(source) || anyNA(out)) {
        return(NULL)
    }
    as.integer(out)
}


sanitize_unconditional_vcov_arg <- function(vcov, modeldata, auxdata) {
    if (isTRUE(checkmate::check_formula(vcov))) {
        cluster_var <- all.vars(vcov)
        if (length(vcov) != 2L || length(cluster_var) != 1L) {
            msg <- paste0(
                "Unconditional variance currently supports a one-sided ",
                "formula for one-way clustered inference only."
            )
            stop_sprintf(msg)
        }
        if (cluster_var %in% colnames(modeldata)) {
            cluster <- modeldata[[cluster_var]]
        } else if (cluster_var %in% colnames(auxdata)) {
            cluster <- auxdata[[cluster_var]]
        } else {
            stop_sprintf(
                "Cluster variable \"%s\" was not found in the model data.",
                cluster_var
            )
        }
        if (length(cluster) != nrow(modeldata)) {
            stop_sprintf(
                "Cluster variable \"%s\" has length %d, but the model data have %d rows.",
                cluster_var,
                length(cluster),
                nrow(modeldata)
            )
        }
        if (anyNA(cluster)) {
            stop_sprintf("Cluster variable \"%s\" contains missing values.", cluster_var)
        }
        if (length(unique(cluster)) < 2) {
            stop_sprintf("Cluster-robust unconditional variance requires at least two clusters.")
        }
        return(list(type = "cluster", cluster = cluster, cluster_var = cluster_var))
    }

    msg <- paste0(
        "Unconditional variance requires `vcov` to be one of ",
        "\"HC1\", \"robust\", \"HC0\", or a one-sided cluster formula."
    )
    if (!isTRUE(checkmate::check_character(vcov, len = 1))) {
        stop_sprintf(msg)
    }
    vcov_lower <- tolower(vcov)
    if (!vcov_lower %in% c("robust", "hc1", "hc0")) {
        stop_sprintf(msg)
    }
    if (vcov_lower == "hc0") {
        list(type = "HC0", cluster = NULL, cluster_var = NULL)
    } else {
        list(type = "HC1", cluster = NULL, cluster_var = NULL)
    }
}


get_unconditional_auxdata <- function(mfx, n) {
    auxdata <- data.table::as.data.table(mfx@newdata)
    if ("rowid" %in% colnames(auxdata)) {
        auxdata <- auxdata[!duplicated(rowid)]
    } else if (nrow(auxdata) > n) {
        auxdata <- auxdata[seq_len(n)]
    }
    if (nrow(auxdata) != n) {
        auxdata <- data.table::as.data.table(mfx@modeldata)
    }
    additional_data <- tryCatch(
        data.table::as.data.table(get_modeldata(mfx@model, additional_variables = TRUE)),
        error = function(e) NULL
    )
    if (!is.null(additional_data) && nrow(additional_data) == n) {
        additional_columns <- setdiff(colnames(additional_data), colnames(auxdata))
        if (length(additional_columns) > 0) {
            auxdata <- cbind(auxdata, additional_data[, additional_columns, with = FALSE])
        }
    }
    auxdata
}


is_unconditional_linear_model <- function(model) {
    (inherits(model, "lm") && !inherits(model, "glm")) ||
        (inherits(model, "fixest") && identical(model[["method_type"]], "feols"))
}


sanitize_unconditional_df <- function(df, model, vcov_info) {
    if (is.numeric(df)) {
        checkmate::assert_numeric(df, lower = 1, any.missing = FALSE, min.len = 1)
        return(df)
    }
    checkmate::assert_choice(df, "residual")

    if (!is_unconditional_linear_model(model)) {
        return(Inf)
    }
    if (vcov_info$type == "cluster") {
        return(length(unique(vcov_info$cluster)) - 1)
    }

    out <- tryCatch(stats::df.residual(model), error = function(e) Inf)
    if (is.finite(out) && out < 1) {
        msg <- paste0(
            "`df = \"residual\"` requires positive residual degrees of ",
            "freedom for linear models. Specify a numeric `df`, such as ",
            "`unconditional(\"HC0\", df = Inf)`, to override this default."
        )
        stop_sprintf(msg)
    }
    out
}


unconditional_df_has_finite <- function(df) {
    is.numeric(df) && any(is.finite(df))
}
