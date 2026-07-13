stop_unconditional <- function(reason) {
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


sanitize_unconditional_vcov_request <- function(vcov, mfx) {
    if (!inherits(vcov, "marginaleffects_vcov_unconditional")) {
        stop_sprintf("Internal error: unconditional vcov sanitization requires a `vcovUnconditional()` object.")
    }

    calling_function <- tryCatch(mfx@calling_function, error = function(e) NULL)
    validate_unconditional_model_support(mfx@model, kind = calling_function)

    modeldata <- data.table::as.data.table(mfx@modeldata)
    auxdata <- if (inherits(vcov$cluster, "formula")) {
        get_unconditional_auxdata(mfx, nrow(modeldata))
    }
    vcov_info <- sanitize_unconditional_vcov_arg(
        type = vcov$type,
        cluster = vcov$cluster,
        modeldata = modeldata,
        auxdata = auxdata
    )
    structure(
        list(vcov = vcov_info),
        class = "marginaleffects_vcov_unconditional"
    )
}


validate_unconditional_model_support <- function(model, kind) {
    if (inherits(model, c("mira", "amest"))) {
        stop_unconditional("imputation")
    }

    # Deliberately inspect the primary class to reject unvalidated subclasses
    # such as `mlm`, even when they also inherit from a supported class.
    primary_class <- class(model)[1]

    # Hansen–Overgaard do not explicitly validate censored or survival models.
    # Their estimating systems can also contain nuisance parameters whose
    # influence is not represented by the current coefficient-only Jacobian.
    if (inherits(model, c("tobit", "survreg", "coxph"))) {
        msg <- paste0(
            "`vcov = \"unconditional\"` is not supported for censored or ",
            "survival models of class \"%s\". These models are outside the ",
            "currently validated conditional-mean setup and can require ",
            "additional nuisance-parameter influence functions. Use a ",
            "bootstrap method instead."
        )
        stop_sprintf(msg, primary_class)
    }

    if (isTRUE(primary_class %in% c("lm", "glm"))) {
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

    reason <- paste0(
        "only explicitly validated model classes are supported. ",
        "Currently supported classes include `lm`, `glm`, and selected ",
        "`fixest` models"
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


validate_unconditional_fixest_plan <- function(model, plan) {
    if (
        !inherits(model, "fixest") ||
            is.null(model[["fixef_vars"]]) ||
            !identical(plan$kind, "comparisons")
    ) {
        return(invisible(TRUE))
    }

    # `fixest::estfun()` and `fixest::bread()` do not expose the individual
    # absorbed fixed-effect coefficients. For linear `feols` models this is
    # harmless when the estimand depends only on hi - lo: the same additive
    # fixed effect appears in both predictions and cancels exactly. dydx and
    # dyex are rescalings of that difference and inherit the same invariance.
    # Ratios, prediction-level elasticities, expdydx, and arbitrary functions
    # can depend on the prediction levels, so fixed-effect uncertainty need not
    # cancel and those comparisons are rejected conservatively.
    allowed <- c(
        "difference", "differenceavg", "differenceavgwts",
        "dydx", "dydxavg", "dydxavgwts",
        "dyex", "dyexavg", "dyexavgwts"
    )
    fun_keys <- vapply(plan$groups, function(group) {
        key <- group$fun_key
        if (length(key) != 1L || is.na(key)) NA_character_ else key
    }, character(1))
    invalid <- unique(fun_keys[is.na(fun_keys) | !fun_keys %in% allowed])
    if (length(invalid) > 0L) {
        labels <- ifelse(is.na(invalid), "custom function", sprintf("`%s`", invalid))
        stop_sprintf(
            paste0(
                "`vcov = \"unconditional\"` supports `feols` models with fixed effects only for ",
                "additive differences and `dydx` or `dyex` slopes, where the fixed effects cancel. ",
                "Unsupported comparison: %s. Use a supported comparison or a bootstrap method."
            ),
            toString(labels)
        )
    }

    # Use the parsed fixed-effect formula rather than `fixef_vars`: the latter
    # stores combined effects such as `id^time` as one string. `all.vars()` also
    # extracts the slope variable from varying-slope terms such as `id[x]`.
    fixef_formula <- tryCatch(model[["fml_all"]][["fixef"]], error = function(e) NULL)
    fixef_variables <- tryCatch(all.vars(fixef_formula), error = function(e) character())
    if (length(fixef_variables) == 0L) {
        stop_sprintf(
            paste0(
                "`vcov = \"unconditional\"` could not identify the fixed-effect variables in this ",
                "`feols` model, so it cannot verify that fixed effects cancel. Use a bootstrap method."
            )
        )
    }

    hi <- plan$predict_args$hi
    lo <- plan$predict_args$lo
    missing <- setdiff(fixef_variables, intersect(colnames(hi), colnames(lo)))
    if (length(missing) > 0L) {
        stop_sprintf(
            paste0(
                "`vcov = \"unconditional\"` could not verify these fixed-effect or varying-slope ",
                "variables in the counterfactual data: %s. Use a bootstrap method."
            ),
            toString(sprintf("`%s`", missing))
        )
    }

    # Exact equality is intentional. A varying-slope variable can change by a
    # very small finite-difference step, which a tolerance-based comparison
    # could incorrectly treat as unchanged.
    changed <- fixef_variables[vapply(
        fixef_variables,
        function(variable) !identical(hi[[variable]], lo[[variable]]),
        logical(1)
    )]
    if (length(changed) > 0L) {
        stop_sprintf(
            paste0(
                "`vcov = \"unconditional\"` cannot vary fixed-effect or varying-slope variables ",
                "because their uncertainty does not cancel: %s. Use a different focal variable or ",
                "a bootstrap method."
            ),
            toString(sprintf("`%s`", changed))
        )
    }

    invisible(TRUE)
}


sanitize_unconditional_plan <- function(
    plan,
    model,
    modeldata,
    n,
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
    validate_unconditional_fixest_plan(model, plan)

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


sanitize_unconditional_vcov_arg <- function(type, cluster, modeldata, auxdata) {
    type_choices <- c("HC", "HC0", "HC1", "HC2", "HC3", "HC4", "HC4m", "HC5")
    type_message <- paste0(
        "`type` must be one of ",
        paste(sprintf("\"%s\"", type_choices), collapse = ", "),
        ", or \"robust\"."
    )
    if (!isTRUE(checkmate::check_character(type, len = 1, any.missing = FALSE))) {
        stop_sprintf(type_message)
    }
    if (tolower(type) == "robust") {
        type <- "HC1"
    } else {
        idx <- match(tolower(type), tolower(type_choices))
        if (is.na(idx)) {
            stop_sprintf(type_message)
        }
        type <- type_choices[idx]
        if (type == "HC") {
            type <- "HC0"
        }
    }

    if (!is.null(cluster) && type %in% c("HC2", "HC3", "HC4", "HC4m", "HC5")) {
        stop_sprintf(
            "`vcovUnconditional()` does not currently support `type = \"%s\"` with `cluster`. Use `type = \"HC0\"` or `type = \"HC1\"` for clustered inference.",
            type
        )
    }

    cluster_var <- NULL
    cluster_values <- NULL
    if (!is.null(cluster)) {
        if (!isTRUE(checkmate::check_formula(cluster))) {
            stop_sprintf("`cluster` must be NULL or a one-sided formula such as `~id`.")
        }
        cluster_var <- all.vars(cluster)
        if (length(cluster) != 2L || length(cluster_var) != 1L) {
            msg <- paste0(
                "Unconditional variance currently supports a one-sided ",
                "formula for one-way clustered inference only."
            )
            stop_sprintf(msg)
        }
        if (cluster_var %in% colnames(modeldata)) {
            cluster_values <- modeldata[[cluster_var]]
        } else if (cluster_var %in% colnames(auxdata)) {
            cluster_values <- auxdata[[cluster_var]]
        } else {
            stop_sprintf(
                "Cluster variable \"%s\" was not found in the model data.",
                cluster_var
            )
        }
        if (length(cluster_values) != nrow(modeldata)) {
            stop_sprintf(
                "Cluster variable \"%s\" has length %d, but the model data have %d rows.",
                cluster_var,
                length(cluster_values),
                nrow(modeldata)
            )
        }
        if (anyNA(cluster_values)) {
            stop_sprintf("Cluster variable \"%s\" contains missing values.", cluster_var)
        }
        if (length(unique(cluster_values)) < 2) {
            stop_sprintf("Cluster-robust unconditional variance requires at least two clusters.")
        }
    }

    list(type = type, cluster = cluster_values, cluster_var = cluster_var)
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
