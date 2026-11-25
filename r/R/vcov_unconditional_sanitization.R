stop_unconditional <- function(reason) {
    msg <- switch(
        reason,
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

    # `glm_weightit` (and its `lm_weightit` alias, which shares the class),
    # `multinom_weightit`, and `ordinal_weightit` are included because
    # `WeightIt` supplies `estfun()` and `bread()` methods that already fold
    # the weight-estimation correction into the outcome-coefficient scores, so
    # `scores %*% bread` is a complete influence function in the convention
    # used by `get_unconditional_beta_dot()`. `nnet::multinom()` and
    # `MASS::polr()` are maximum-likelihood fits whose `sandwich` scores and
    # breads are named like their coefficients, cutpoints included for `polr`.
    # Multinomial and ordinal models return one block of predictions per
    # outcome level; `sanitize_unconditional_plan()` maps each block back to
    # the source rows. `coxph_weightit` is rejected by the survival guard above.
    weightit_classes <- c("glm_weightit", "multinom_weightit", "ordinal_weightit")
    categorical_classes <- c("multinom", "polr")
    supported <- c("lm", "glm", "svyglm", weightit_classes, categorical_classes)
    if (isTRUE(primary_class %in% supported)) {
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
        "Currently supported classes include `lm`, `glm`, `svyglm`, ",
        "`glm_weightit`, `multinom_weightit`, `ordinal_weightit`, `multinom`, ",
        "`polr`, and selected `fixest` models"
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
    allow_mismatch = character(),
    newdata_index = NULL) {

    if (identical(plan$kind, "predictions")) {
        source <- plan$predict_args$newdata
        n_pred <- plan$n_pred
    } else {
        source <- plan$predict_args$original
        n_pred <- length(plan$baseline_hi)
    }

    msg <- paste0(
        "`vcov = \"unconditional\"` requires effects evaluated over ",
        "original model-data rows, a subset of original rows with valid ",
        "row IDs, or a full counterfactual grid that preserves `rowidcf`. ",
        "To target a subset of the model data, such as the treated units, ",
        "use `newdata = subset(...)` or `by`."
    )

    # `original` is NULL for `cross = TRUE` comparisons.
    if (!inherits(source, "data.frame")) {
        stop_sprintf(msg)
    }

    # Map every prediction row to the source row it was computed from. Models
    # with several outcome levels return one block of predictions per level,
    # each repeating the source rows in order; this is the layout `add_rowid()`
    # assumes, and it has been checked for the classes below. Any other model
    # must return one prediction per source row. Mapping before `keep` matters
    # because `keep` indexes prediction rows, not source rows.
    n_source <- nrow(source)
    block_classes <- c("multinom", "polr", "multinom_weightit", "ordinal_weightit")
    if (length(n_pred) != 1L || n_pred == n_source) {
        source_idx <- seq_len(n_source)
    } else if (
        isTRUE(class(model)[1] %in% block_classes) &&
            n_source > 0L &&
            n_pred %% n_source == 0L
    ) {
        source_idx <- rep(seq_len(n_source), times = n_pred %/% n_source)
    } else {
        stop_sprintf(
            "Internal error: %d predictions cannot be mapped to %d source rows for `vcov = \"unconditional\"`.",
            n_pred,
            n_source
        )
    }
    if (identical(plan$kind, "predictions") && !is.null(plan$keep)) {
        source_idx <- source_idx[plan$keep]
    }
    used <- unique(source_idx)

    rowid <- resolve_unconditional_rowid(
        source[used, , drop = FALSE],
        n,
        modeldata = modeldata,
        allow_mismatch = allow_mismatch,
        message = msg,
        newdata_index = newdata_index
    )
    rowid <- rowid[match(source_idx, used)]

    validate_unconditional_plan_target(plan)
    validate_unconditional_fixest_plan(model, plan)

    rowid
}


# Map the rows of `mfx@newdata` to model-data rows, keyed by `newdata$rowid`.
# Returns NULL, so that row resolution proceeds as if this map did not exist,
# whenever some rows cannot be mapped or `rowid` does not identify rows.
get_unconditional_newdata_index <- function(mfx) {
    newdata <- tryCatch(mfx@newdata, error = function(e) NULL)
    if (!inherits(newdata, "data.frame") || !"rowid" %in% colnames(newdata)) {
        return(NULL)
    }
    # Padding rows (negative `rowid`) only supply factor levels and are dropped
    # from the estimates, so they have no model-data row.
    newdata <- drop_unconditional_padding(newdata)
    # A user-supplied `rowid` column need not identify rows.
    if (is.null(newdata) || anyDuplicated(newdata[["rowid"]]) > 0L) {
        return(NULL)
    }
    index <- newdata_modelrow_index(mfx, newdata)
    if (is.null(index)) {
        return(NULL)
    }
    list(rowid = newdata[["rowid"]], index = index)
}


drop_unconditional_padding <- function(newdata) {
    newdata <- as.data.frame(newdata)
    rowid <- newdata[["rowid"]]
    if (!is.numeric(rowid) || anyNA(rowid)) {
        return(NULL)
    }
    newdata[which(rowid > 0), , drop = FALSE]
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
    message = NULL,
    newdata_index = NULL) {

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

    # Trust a candidate mapping only if the rows it points to hold the same
    # values as the evaluation rows.
    rowid_values_match <- function(candidate) {
        if (is.null(modeldata)) {
            return(TRUE)
        }
        all(vapply(common, function(column) {
            isTRUE(all.equal(
                x[[column]],
                modeldata[[column]][candidate],
                check.attributes = FALSE
            ))
        }, logical(1)))
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
        if (rowid_values_match(candidate)) {
            out <- candidate
        }
        if (!is.null(out)) {
            break
        }
    }

    # A subset of the model data, such as `newdata = subset(treat == 1)` for
    # the ATT, carries `rowid = seq_len(nrow(newdata))`, which says nothing
    # about the model-data rows. `newdata_index` maps each `newdata` row to the
    # model-data row it was taken from, as recorded when the `subset()` or
    # `filter()` call was evaluated on the model data; the source rows reach it
    # through their `rowid`.
    if (is.null(out) &&
        !is.null(newdata_index) &&
        "rowid" %in% colnames(x)) {
        candidate <- newdata_index$index[match(x[["rowid"]], newdata_index$rowid)]
        if (!anyNA(candidate) && rowid_values_match(candidate)) {
            out <- as.integer(candidate)
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
    if (anyDuplicated(model_keys, by = common) > 0) {
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
    type_choices <- c("HC0", "HC1")
    type_message <- paste0(
        "`type` must be one of ",
        paste(sprintf("\"%s\"", type_choices), collapse = ", "),
        "."
    )
    if (!isTRUE(checkmate::check_character(type, len = 1, any.missing = FALSE))) {
        stop_sprintf(type_message)
    }
    if (toupper(type) %in% c("HC2", "HC3", "HC4", "HC4M", "HC5")) {
        stop_sprintf(
            "`type = \"%s\"` is not available for unconditional inference because regression-leverage adjustments are not defined for the combined influence function. Use `type = \"HC0\"` or `type = \"HC1\"`.",
            type
        )
    }
    idx <- match(tolower(type), tolower(type_choices))
    if (is.na(idx)) {
        stop_sprintf(type_message)
    }
    type <- type_choices[idx]

    cluster_var <- NULL
    cluster_values <- NULL
    if (!is.null(cluster)) {
        cluster_message <- paste0(
            "`cluster` must be NULL or a one-sided formula with one bare variable name, ",
            "such as `~id`, for one-way clustered inference. Transformations and ",
            "multiple variables are not supported."
        )
        if (
            !isTRUE(checkmate::check_formula(cluster)) ||
                length(cluster) != 2L ||
                !is.symbol(cluster[[2L]])
        ) {
            stop_sprintf(cluster_message)
        }
        # Do not use `all.vars()` here: it would accept expressions such as
        # `~floor(id / 10)` and then silently extract the untransformed `id`.
        # Requiring a symbol makes the formula an unambiguous column reference.
        cluster_var <- as.character(cluster[[2L]])
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
