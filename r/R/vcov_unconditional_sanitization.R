stop_unconditional <- function(
    reason = NULL,
    vcov = NULL,
    model = NULL,
    command = NULL,
    kind = NULL,
    type = NULL) {

    if (is.null(reason)) {
        if (!is_unconditional_vcov(vcov) && !is_unconditional_vcov_call(vcov)) {
            return(invisible(TRUE))
        } else if (!is.null(model) && inherits(model, c("mira", "amest"))) {
            reason <- "imputation"
        } else if (isTRUE(command %in% c("hypotheses", "joint_test"))) {
            reason <- "hypotheses"
        } else if (identical(command, "inferences")) {
            reason <- "inferences"
        }
    }

    if (is.null(reason)) {
        if (!is.null(model) && !is.null(kind)) {
            validate_unconditional_model_support(model, kind = kind, type = type)
        }
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

    stop_unconditional(
        vcov = vcov,
        model = mfx@model,
        kind = tryCatch(mfx@calling_function, error = function(e) NULL)
    )

    if (inherits(vcov, "marginaleffects_vcov_unconditional")) {
        vc <- vcov$vcov
        df <- vcov$df
    } else {
        vc <- "HC1"
        if (isTRUE(df_supplied)) {
            calling_function <- tryCatch(mfx@calling_function, error = function(e) NULL)
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
    auxdata <- get_unconditional_auxdata(mfx, nrow(modeldata))
    vcov_info <- sanitize_unconditional_vcov_arg(vc, modeldata, auxdata)
    df_value <- sanitize_unconditional_df(df, mfx@model, vcov_info)

    structure(
        list(vcov = vcov_info, df = df_value),
        class = "marginaleffects_vcov_unconditional"
    )
}


validate_unconditional_model_support <- function(model, kind, type = NULL) {
    cls <- class(model)[1]
    if (isTRUE(cls %in% c("lm", "glm", "survreg", "tobit"))) {
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

    stop_unconditional_unsupported_model(model)
}


stop_unconditional_unsupported_model <- function(model, reason = NULL) {
    if (is.null(reason)) {
        reason <- paste0(
            "only explicitly validated model classes are supported. ",
            "Currently supported classes include `lm`, `glm`, selected ",
            "`fixest`, selected survival models, and `tobit` models"
        )
    }
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


has_unconditional_scalar_comparison <- function(plan) {
    if (!identical(plan$kind, "comparisons")) {
        return(FALSE)
    }
    scalar_group <- vapply(plan$groups, function(g) {
        isTRUE(g$scalar) && length(g$idx) > 1L
    }, logical(1))
    any(scalar_group)
}


validate_unconditional_plan_target <- function(plan) {
    scalar_comparison <- has_unconditional_scalar_comparison(plan)

    if (!is.null(plan$agg)) {
        return(invisible(TRUE))
    }

    if (!is.null(plan$hyp)) {
        if (isTRUE(scalar_comparison)) {
            return(invisible(TRUE))
        }
        msg <- paste0(
            "`vcov = \"unconditional\"` does not support `hypothesis` ",
            "applied directly to unit-level effects. Use an `avg_*()` ",
            "function, a `by` argument, or a scalar comparison before ",
            "`hypothesis`."
        )
        stop_sprintf(msg)
    }

    if (isTRUE(scalar_comparison)) {
        return(invisible(TRUE))
    }

    msg <- paste0(
        "`vcov = \"unconditional\"` is only supported for averaged or ",
        "aggregated effects. Use an `avg_*()` function, a `by` argument, ",
        "or a scalar comparison."
    )
    stop_sprintf(msg)
}


validate_unconditional_plan_source <- function(plan, modeldata, n, allow_mismatch = character()) {
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
    resolve_unconditional_rowid(
        source,
        n,
        modeldata = modeldata,
        allow_mismatch = allow_mismatch,
        message = msg)
    invisible(TRUE)
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
            matches <- isTRUE(unconditional_source_matches_modeldata(
                x,
                modeldata,
                candidate,
                allow_mismatch = allow_mismatch))
            if (isTRUE(matches)) {
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
            allow_mismatch = allow_mismatch)
        if (!is.null(matched)) {
            out <- matched
        }
    }

    if (is.null(out) || anyNA(out) || any(out < 1L) || any(out > n)) {
        stop_sprintf(message)
    }
    as.integer(out)
}


unconditional_source_matches_modeldata <- function(source, modeldata, rowid, allow_mismatch = character()) {
    common <- intersect(colnames(source), colnames(modeldata))
    common <- setdiff(common, c(
        "rowid",
        "rowidcf",
        "marginaleffects_wts_internal",
        "rowid_dedup"
    ))
    common <- setdiff(common, allow_mismatch)
    if (length(common) == 0) {
        return(TRUE)
    }

    rowid <- as.integer(rowid)
    for (col in common) {
        lhs <- source[[col]]
        rhs <- modeldata[[col]][rowid]
        if (!isTRUE(all.equal(lhs, rhs, check.attributes = FALSE))) {
            return(FALSE)
        }
    }
    TRUE
}


match_unconditional_source_modeldata <- function(source, modeldata, allow_mismatch = character()) {
    source <- data.table::as.data.table(source)
    modeldata <- data.table::as.data.table(modeldata)
    common <- intersect(colnames(source), colnames(modeldata))
    common <- setdiff(common, c(
        "rowid",
        "rowidcf",
        "marginaleffects_wts_internal",
        "rowid_dedup"
    ))
    common <- setdiff(common, allow_mismatch)
    if (length(common) == 0) {
        return(NULL)
    }

    md <- data.table::copy(modeldata[, common, with = FALSE])
    if (any(duplicated(md, by = common))) {
        return(NULL)
    }
    md[, ".marginaleffects_unconditional_rowid" := seq_len(.N)]
    src <- source[, common, with = FALSE]
    out <- md[src, on = common][[".marginaleffects_unconditional_rowid"]]
    if (length(out) != nrow(source) || anyNA(out)) {
        return(NULL)
    }
    as.integer(out)
}


sanitize_unconditional_vcov_arg <- function(vcov, modeldata, auxdata) {
    if (isTRUE(checkmate::check_formula(vcov))) {
        cluster_var <- all.vars(vcov)
        if (length(cluster_var) != 1) {
            msg <- "Unconditional variance currently supports one-way clustered inference only."
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

    if (!isTRUE(checkmate::check_character(vcov, len = 1))) {
        msg <- paste0(
            "Unconditional variance requires `vcov` to be one of ",
            "\"HC1\", \"robust\", \"HC0\", or a one-sided cluster formula."
        )
        stop_sprintf(msg)
    }
    vcov_lower <- tolower(vcov)
    if (!vcov_lower %in% c("robust", "hc1", "hc0")) {
        msg <- paste0(
            "Unconditional variance requires `vcov` to be one of ",
            "\"HC1\", \"robust\", \"HC0\", or a one-sided cluster formula."
        )
        stop_sprintf(msg)
    }
    if (vcov_lower == "hc0") {
        list(type = "HC0", cluster = NULL, cluster_var = NULL)
    } else {
        list(type = "HC1", cluster = NULL, cluster_var = NULL)
    }
}


get_unconditional_auxdata <- function(mfx, n) {
    nd <- data.table::as.data.table(mfx@newdata)
    if ("rowid" %in% colnames(nd)) {
        nd <- nd[!duplicated(rowid)]
    } else if (nrow(nd) > n) {
        nd <- nd[seq_len(n)]
    }
    if (nrow(nd) != n) {
        nd <- data.table::as.data.table(mfx@modeldata)
    }
    extra <- tryCatch(
        data.table::as.data.table(get_modeldata(mfx@model, additional_variables = TRUE)),
        error = function(e) NULL
    )
    if (!is.null(extra) && nrow(extra) == n) {
        add <- setdiff(colnames(extra), colnames(nd))
        if (length(add) > 0) {
            nd <- cbind(nd, extra[, add, with = FALSE])
        }
    }
    nd
}


sanitize_unconditional_df <- function(df, model, vcov_info) {
    if (is.numeric(df)) {
        checkmate::assert_numeric(df, lower = 1, any.missing = FALSE, min.len = 1)
        return(df)
    }
    checkmate::assert_choice(df, "residual")

    is_glm <- inherits(model, "glm")
    is_fixest <- inherits(model, "fixest")
    is_feols <- is_fixest && identical(model$method_type, "feols")

    if (vcov_info$type == "cluster" && (inherits(model, "lm") && !is_glm || is_feols)) {
        return(length(unique(vcov_info$cluster)) - 1)
    }
    if (vcov_info$type != "cluster" && (inherits(model, "lm") && !is_glm || is_feols)) {
        out <- tryCatch(stats::df.residual(model), error = function(e) Inf)
        if (is.finite(out) && out < 1) {
            msg <- paste0(
                "`df = \"residual\"` requires positive residual degrees of ",
                "freedom for linear models. Specify a numeric `df`, such as ",
                "`unconditional(\"HC0\", df = Inf)`, to override this default."
            )
            stop_sprintf(msg)
        }
        return(out)
    }
    Inf
}


unconditional_df_has_finite <- function(df) {
    is.numeric(df) && any(is.finite(df))
}


unconditional_df_all_infinite <- function(df) {
    is.numeric(df) && all(!is.finite(df))
}


validate_unconditional_df_length <- function(df, n) {
    if (is.numeric(df) && !length(df) %in% c(1L, n)) {
        stop_sprintf(
            "The `df` argument must have length 1 or match the number of estimates (%d). Got length %d.",
            n,
            length(df)
        )
    }
    invisible(TRUE)
}
