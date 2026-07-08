#' Request unconditional variance in marginaleffects calls
#'
#' @param vcov Variance estimator for the linearized scores. Supported values
#'   are `"HC1"`, `"robust"`, `"HC0"`, or a one-sided formula such
#'   as `~id` for one-way clustered inference.
#' @param df Degrees of freedom. `"residual"` uses residual degrees of freedom
#'   for linear robust models, cluster count minus one for linear clustered
#'   models, and normal inference for GLMs.
#'
#' @return An object which can be supplied to the `vcov` argument of
#'   [avg_predictions()], [avg_comparisons()], or [avg_slopes()].
#'
#' @details
#' Unconditional variance is available for effects evaluated over original
#' model-data rows, valid subsets of those rows, or counterfactual grids that
#' preserve a valid `rowid`/`rowidcf` mapping to original model-data rows. The
#' effect must be averaged or aggregated with `avg_*()` or `by`, or it must be a
#' scalar comparison. Hypotheses applied directly to unit-level effects are
#' rejected because the empirical-distribution influence function is not
#' identifiable from an arbitrary post-hoc hypothesis function. Synthetic grids
#' such as `newdata = "mean"` are rejected because the current implementation
#' cannot generally infer how those grid values vary with the empirical
#' covariate distribution. Models must provide compatible score and bread
#' matrices through `sandwich::estfun()`/`sandwich::bread()` or model-specific
#' equivalents. Multiple-imputation objects, posterior-draw models,
#' survey-design models, mixed-effects models, multi-equation and multinomial
#' choice models, baseline-hazard dependent `coxph` predictions such as
#' `type = "survival"` or `type = "expected"`, average predictions from
#' `fixest` models with fixed effects, and nonlinear `fixest` models with fixed
#' effects are rejected explicitly.
#'
#' @export
unconditional <- function(vcov = "HC1", df = "residual") {
    structure(
        list(vcov = vcov, df = df),
        class = "marginaleffects_vcov_unconditional"
    )
}


is_unconditional_vcov <- function(vcov) {
    inherits(vcov, "marginaleffects_vcov_unconditional") ||
        (is.character(vcov) && length(vcov) == 1 && tolower(vcov) == "unconditional")
}


is_unconditional_vcov_call <- function(vcov) {
    if (!is.call(vcov)) {
        return(FALSE)
    }
    fun <- tryCatch(as.character(vcov[[1]]), error = function(e) character())
    length(fun) > 0L && identical(fun[[length(fun)]], "unconditional")
}


stop_unconditional_imputation <- function() {
    msg <- paste0(
        "`vcov = \"unconditional\"` is not supported for ",
        "multiple-imputation model objects or pooled multiple-imputation results."
    )
    stop_sprintf(msg)
}


stop_unconditional_hypotheses <- function() {
    msg <- paste0(
        "`vcov = \"unconditional\"` is only available for predictions, ",
        "comparisons, and slopes computed by `marginaleffects`. Use ",
        "`avg_predictions()`, `avg_comparisons()`, or `avg_slopes()` with ",
        "`vcov = \"unconditional\"`, then call `hypotheses()` on the result ",
        "if needed."
    )
    stop_sprintf(msg)
}


stop_unconditional_inferences <- function() {
    msg <- paste0(
        "`inferences()` is not available for objects computed with ",
        "`vcov = \"unconditional\"`. Use the unconditional standard errors ",
        "from the original `avg_*()` call, or refit the original call with ",
        "`vcov = FALSE` before applying `inferences()`."
    )
    stop_sprintf(msg)
}


validate_unconditional_request <- function(vcov, model = NULL, command = NULL, kind = NULL, type = NULL) {
    if (!is_unconditional_vcov(vcov) && !is_unconditional_vcov_call(vcov)) {
        return(invisible(TRUE))
    }

    if (!is.null(model) && inherits(model, c("mira", "amest"))) {
        stop_unconditional_imputation()
    }

    if (isTRUE(command %in% c("hypotheses", "joint_test"))) {
        stop_unconditional_hypotheses()
    }

    if (identical(command, "inferences")) {
        stop_unconditional_inferences()
    }

    if (!is.null(model) && !is.null(kind)) {
        validate_unconditional_model_support(model, kind = kind, type = type)
    }

    invisible(TRUE)
}


update_unconditional_vcov_transform <- function(mfx, estimate, transform) {
    vcov_type <- tryCatch(mfx@vcov_type, error = function(e) "")
    if (!isTRUE(grepl("^Unconditional", vcov_type))) {
        return(mfx)
    }

    if (!is.function(transform)) {
        if (is.null(transform[[1]])) {
            return(mfx)
        }
        transform <- transform[[1]]
    }

    if (is.null(estimate)) {
        return(mfx)
    }

    V <- mfx@vcov_model
    if (!isTRUE(checkmate::check_matrix(V, mode = "numeric"))) {
        return(mfx)
    }
    if (nrow(V) != ncol(V) || length(estimate) != nrow(V)) {
        stop_sprintf(
            "Internal error: unconditional vcov has %d rows but the transformed estimates have length %d.",
            nrow(V),
            length(estimate)
        )
    }

    estimate <- as.numeric(estimate)
    if (anyNA(estimate) || any(!is.finite(estimate))) {
        msg <- paste0(
            "`vcov = \"unconditional\"` cannot transform the saved covariance ",
            "matrix when pre-transform estimates are non-finite."
        )
        stop_sprintf(msg)
    }

    transform_checked <- function(x) {
        out <- transform(x)
        if (!is.numeric(out) || length(out) != length(x)) {
            stop_sprintf(
                "The `transform` function must return a numeric vector with the same length as its input."
            )
        }
        as.numeric(out)
    }

    H <- tryCatch(
        get_jacobian(transform_checked, estimate, mfx@numderiv %||% list("fdforward")),
        error = function(e) {
            stop_sprintf(
                "`vcov = \"unconditional\"` could not numerically differentiate the `transform` function: %s",
                conditionMessage(e)
            )
        }
    )
    if (!identical(dim(H), c(length(estimate), length(estimate))) || anyNA(H) || any(!is.finite(H))) {
        stop_sprintf(
            "`vcov = \"unconditional\"` could not compute a finite transform Jacobian."
        )
    }

    V <- H %*% V %*% t(H)
    V <- (V + t(V)) / 2
    dimnames(V) <- dimnames(mfx@vcov_model)
    mfx@vcov_model <- V
    mfx@jacobian <- diag(nrow(V))
    mfx
}


sanitize_unconditional_vcov_request <- function(vcov, mfx, df = "residual") {
    if (!is_unconditional_vcov(vcov)) {
        return(NULL)
    }

    validate_unconditional_request(
        vcov,
        model = mfx@model,
        kind = tryCatch(mfx@calling_function, error = function(e) NULL)
    )

    if (inherits(vcov, "marginaleffects_vcov_unconditional")) {
        vc <- vcov$vcov
        df <- vcov$df
    } else {
        vc <- "HC1"
    }

    modeldata <- data.table::as.data.table(mfx@modeldata)
    auxdata <- get_unconditional_auxdata(mfx, nrow(modeldata))
    vcov_info <- sanitize_unconditional_vcov_arg(vc, modeldata, auxdata)
    df_value <- sanitize_unconditional_df(df, mfx@model, vcov_info)

    list(vcov = vcov_info, df = df_value)
}


plan_unconditional_se <- function(
    built,
    mfx,
    estimates,
    type,
    unconditional,
    dots = list(),
    contrast_data = NULL,
    variables = NULL,
    numderiv = NULL) {

    if (!is.null(mfx) && !is.null(mfx@draws)) {
        stop_sprintf(
            "`vcov = \"unconditional\"` is not supported for models or prediction methods that return posterior draws."
        )
    }

    inference_cols <- intersect(
        c("std.error", "statistic", "p.value", "s.value", "conf.low", "conf.high", "df"),
        colnames(estimates)
    )
    if (length(inference_cols) > 0) {
        estimates[inference_cols] <- NULL
    }

    plan <- built$plan
    kind <- plan$kind
    if (!isTRUE(kind %in% c("predictions", "comparisons"))) {
        stop_sprintf("Unknown plan kind: %s", kind %||% "NULL")
    }

    model <- mfx@model
    validate_unconditional_model_support(model, kind, type = type)
    modeldata <- data.table::as.data.table(mfx@modeldata)
    n <- nrow(modeldata)
    allow_mismatch <- get_unconditional_allow_mismatch(mfx, variables)
    validate_unconditional_plan_source(plan, modeldata, n, allow_mismatch = allow_mismatch)
    validate_unconditional_plan_target(plan)

    # Unconditional variance adds two influence components:
    # sampling variation in the empirical covariate distribution, and
    # coefficient uncertainty mapped through the existing effect Jacobian.
    beta_dot <- get_unconditional_beta_dot(model)
    if (nrow(beta_dot) != n) {
        stop_sprintf(
            "`vcov = \"unconditional\"` requires one score row per model-data row."
        )
    }

    J <- get_unconditional_plan_jacobian(
        plan = plan,
        mfx = mfx,
        estimates = estimates,
        type = type,
        dots = dots,
        kind = kind,
        contrast_data = contrast_data,
        variables = variables,
        numderiv = numderiv
    )

    if (is.null(colnames(J)) || anyNA(colnames(J)) || any(colnames(J) == "")) {
        stop_sprintf("The unconditional effect Jacobian must have named coefficient columns.")
    }
    missing_beta <- setdiff(colnames(J), colnames(beta_dot))
    missing_jacobian <- setdiff(colnames(beta_dot), colnames(J))
    if (length(missing_beta) > 0 || length(missing_jacobian) > 0) {
        missing <- unique(c(missing_beta, missing_jacobian))
        stop_sprintf(
            "The unconditional score matrix does not match the effect Jacobian. Missing columns: %s.",
            paste(missing, collapse = ", ")
        )
    }
    cols <- colnames(J)
    beta_phi <- beta_dot[, cols, drop = FALSE] %*% t(J[, cols, drop = FALSE])

    empirical_phi <- get_unconditional_plan_empirical_phi(
        plan = plan,
        model = model,
        modeldata = modeldata,
        n = n,
        allow_mismatch = allow_mismatch
    )

    if (ncol(empirical_phi) != nrow(J)) {
        stop_sprintf(
            "Internal error: empirical unconditional influence has %d columns, but the Jacobian has %d rows.",
            ncol(empirical_phi), nrow(J)
        )
    }

    Phi <- empirical_phi + beta_phi
    inputs <- list(
        model = model,
        n = n,
        vcov = unconditional$vcov
    )
    V <- get_unconditional_vcov(Phi, inputs)
    se <- sqrt(diag(V))
    se[se == 0] <- NA_real_

    validate_unconditional_df_length(unconditional$df, nrow(estimates))
    estimates$std.error <- as.vector(se)
    if (unconditional_df_has_finite(unconditional$df)) {
        estimates$df <- unconditional$df
    }

    mfx@vcov_model <- V
    mfx@vcov_type <- if (unconditional$vcov$type == "cluster") {
        sprintf("Unconditional (clustered by %s)", unconditional$vcov$cluster_var)
    } else {
        "Unconditional"
    }
    mfx@df <- unconditional$df
    mfx@jacobian <- diag(nrow(V))

    list(mfx = mfx, estimates = estimates)
}


validate_unconditional_model_support <- function(model, kind, type = NULL) {
    if (is_unconditional_model_supported(model, kind = kind, type = type)) {
        return(invisible(TRUE))
    }

    if (inherits(model, "coxph") && isTRUE(type %in% c("survival", "expected"))) {
        msg <- paste0(
            "`vcov = \"unconditional\"` is not supported for `coxph` ",
            "predictions with `type = \"%s\"` because uncertainty in the ",
            "baseline hazard is not represented. Use `type = \"lp\"`, ",
            "`type = \"risk\"`, or a bootstrap method."
        )
        stop_sprintf(
            msg,
            type
        )
    }

    if (inherits(model, "fixest")) {
        stop_unconditional_unsupported_fixest(model, kind)
    }

    if (inherits(model, c("svyglm", "svyolr", "svy_vglm"))) {
        msg <- paste0(
            "`vcov = \"unconditional\"` is not supported for survey-design ",
            "models. Survey models require a design-based linearization."
        )
        stop_sprintf(msg)
    }

    if (inherits(model, c("brmsfit", "stanreg", "MCMCglmm", "bart", "mvgam"))) {
        stop_unconditional_unsupported_model(
            model,
            "posterior-draw models require a different linearization"
        )
    }

    if (inherits(model, c("merMod", "glmmTMB", "glmmPQL", "lme", "rlmerMod", "clmm", "clmm2"))) {
        stop_unconditional_unsupported_model(
            model,
            "mixed-effects models require treatment of random-effect and variance-component uncertainty"
        )
    }

    if (inherits(model, c(
        "brmultinom", "DirichletRegModel", "mblogit", "mclogit",
        "mhurdle", "mlm", "mlogit", "multinom", "multinom_weightit",
        "nestedLogit", "selection", "systemfit"
    ))) {
        stop_unconditional_unsupported_model(
            model,
            "multi-equation and multinomial models require equation-specific linearization"
        )
    }

    stop_unconditional_unsupported_model(model)
}


is_unconditional_model_supported <- function(model, kind, type = NULL) {
    cls <- class(model)[1]
    if (isTRUE(cls %in% c("lm", "glm", "survreg", "tobit"))) {
        return(TRUE)
    }

    if (inherits(model, "fixest")) {
        return(is_unconditional_fixest_supported(model, kind))
    }

    if (inherits(model, "coxph")) {
        return(is.null(type) || isTRUE(type %in% c("lp", "risk")))
    }

    if (inherits(model, c("survreg", "tobit"))) {
        return(TRUE)
    }

    FALSE
}


is_unconditional_fixest_supported <- function(model, kind) {
    if (is.null(model[["fixef_vars"]])) {
        return(TRUE)
    }
    !identical(kind, "predictions") && identical(model[["method_type"]], "feols")
}


stop_unconditional_unsupported_fixest <- function(model, kind) {
    if (!is.null(model[["fixef_vars"]]) && identical(kind, "predictions")) {
        msg <- paste0(
            "`vcov = \"unconditional\"` is not supported for average ",
            "predictions from `fixest` models with fixed effects because ",
            "fixed-effect uncertainty is not represented."
        )
        stop_sprintf(msg)
    }

    if (!is.null(model[["fixef_vars"]]) && !identical(model[["method_type"]], "feols")) {
        msg <- paste0(
            "`vcov = \"unconditional\"` is not supported for nonlinear ",
            "`fixest` models with fixed effects because fixed-effect ",
            "uncertainty is not represented."
        )
        stop_sprintf(msg)
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


has_unconditional_scalar_comparison <- function(plan) {
    if (!identical(plan$kind, "comparisons")) {
        return(FALSE)
    }
    scalar_group <- vapply(plan$groups, function(g) {
        isTRUE(g$scalar) && length(g$idx) > 1L
    }, logical(1))
    any(scalar_group)
}


get_unconditional_plan_jacobian <- function(
    plan,
    mfx,
    estimates,
    type,
    dots,
    kind,
    contrast_data = NULL,
    variables = NULL,
    numderiv = NULL) {

    coefs <- get_coef(mfx@model)
    V <- diag(length(coefs))
    dimnames(V) <- list(names(coefs), names(coefs))

    if (identical(kind, "predictions")) {
        fun <- function(model_perturbed, ...) {
            pred <- prediction_plan_predict(plan, model_perturbed, ...)
            prediction_plan_apply(plan, pred)
        }
        args <- list(
            mfx = mfx,
            model_perturbed = mfx@model,
            vcov = V,
            type = type,
            FUN = fun,
            hypothesis = mfx@hypothesis
        )
    } else {
        fun <- function(model_perturbed, ...) {
            preds <- comparison_plan_predict(plan, model_perturbed, ...)
            comparison_plan_apply(plan, preds$hi, preds$lo, preds$or)
        }
        args <- list(
            mfx = mfx,
            model_perturbed = mfx@model,
            vcov = V,
            type = type,
            FUN = fun,
            variables = variables,
            hypothesis = mfx@hypothesis,
            hi = contrast_data$hi,
            lo = contrast_data$lo,
            original = contrast_data$original,
            estimates = estimates,
            numderiv = numderiv
        )
    }

    args <- utils::modifyList(args, dots)
    se <- do_call(get_se_delta, args)
    J <- attr(se, "jacobian")
    if (!isTRUE(checkmate::check_matrix(J, mode = "numeric", nrows = nrow(estimates)))) {
        stop_sprintf("Unable to compute the unconditional effect Jacobian.")
    }
    J
}


get_unconditional_plan_empirical_phi <- function(plan, model, modeldata, n, allow_mismatch = character()) {
    if (identical(plan$kind, "predictions")) {
        return(get_unconditional_prediction_plan_phi(plan, model, modeldata, n, allow_mismatch = allow_mismatch))
    }
    get_unconditional_comparison_plan_phi(plan, model, modeldata, n, allow_mismatch = allow_mismatch)
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

    rowid <- get_unconditional_source_rowid(
        source,
        n,
        modeldata = modeldata,
        allow_mismatch = allow_mismatch)
    valid <- !is.null(rowid) &&
        !anyNA(rowid) &&
        all(rowid %in% seq_len(n))

    if (!isTRUE(valid)) {
        msg <- paste0(
            "`vcov = \"unconditional\"` requires effects evaluated over ",
            "original model-data rows, a subset of original rows with valid ",
            "row IDs, or a full counterfactual grid that preserves `rowidcf`."
        )
        stop_sprintf(msg)
    }
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


get_unconditional_prediction_plan_phi <- function(plan, model, modeldata, n, allow_mismatch = character()) {
    pred <- prediction_plan_predict(plan, model)
    source <- plan$predict_args$newdata
    if (!is.null(plan$keep)) {
        pred <- pred[plan$keep]
        source <- source[plan$keep, , drop = FALSE]
    }
    rowid <- get_unconditional_source_rowid(
        source,
        n,
        modeldata = modeldata,
        allow_mismatch = allow_mismatch)
    rowid <- sanitize_unconditional_rowid(rowid, n)

    phi <- get_unconditional_aggregate_phi(
        agg = plan$agg,
        base_est = pred,
        rowid = rowid,
        n = n
    )

    apply_unconditional_hypothesis_phi(phi, plan, pred)
}


get_unconditional_comparison_plan_phi <- function(plan, model, modeldata, n, allow_mismatch = character()) {
    base <- get_unconditional_comparison_base(plan, model, modeldata, n, allow_mismatch = allow_mismatch)

    phi <- get_unconditional_aggregate_phi(
        agg = plan$agg,
        base_est = base$estimate,
        rowid = base$rowid,
        n = n,
        base_phi = base$phi
    )

    apply_unconditional_hypothesis_phi(phi, plan, base$estimate)
}


get_unconditional_comparison_base <- function(plan, model, modeldata, n, allow_mismatch = character()) {
    preds <- comparison_plan_predict(plan, model)
    hi <- preds$hi
    lo <- preds$lo
    y <- preds$or
    rowid <- get_unconditional_source_rowid(
        plan$predict_args$original,
        n,
        modeldata = modeldata,
        allow_mismatch = allow_mismatch)

    if (!is.null(plan$na_keep)) {
        hi <- hi[plan$na_keep]
        lo <- lo[plan$na_keep]
        if (!is.null(y)) {
            y <- y[plan$na_keep]
        }
        rowid <- rowid[plan$na_keep]
    }
    if (!is.null(plan$perm)) {
        hi <- hi[plan$perm]
        lo <- lo[plan$perm]
        if (!is.null(y)) {
            y <- y[plan$perm]
        }
        rowid <- rowid[plan$perm]
    }
    rowid <- sanitize_unconditional_rowid(rowid, n)

    est <- numeric(plan$n_comp)
    est_rowid <- rep(NA_integer_, plan$n_comp)
    phi <- matrix(0, nrow = n, ncol = plan$n_comp)

    for (g in plan$groups) {
        args <- g$args
        args$hi <- hi[g$idx]
        args$lo <- lo[g$idx]
        if (isTRUE(g$uses_y)) {
            args$y <- y[g$idx]
        }
        con <- do_call(g$fun, args)
        if (length(con) != length(g$out_idx)) {
            stop_sprintf("Internal error: comparison plan group changed shape.")
        }

        est[g$out_idx] <- con
        if (length(con) == length(g$idx)) {
            est_rowid[g$out_idx] <- rowid[g$idx]
        } else if (length(con) == 1L && length(g$idx) > 1L) {
            phi[, g$out_idx] <- get_unconditional_scalar_comparison_phi(
                group = g,
                hi = hi,
                lo = lo,
                y = y,
                rowid = rowid,
                n = n,
                theta = con
            )
        }
    }

    if (!is.null(plan$est_keep)) {
        est <- est[plan$est_keep]
        est_rowid <- est_rowid[plan$est_keep]
        phi <- phi[, plan$est_keep, drop = FALSE]
    }

    list(estimate = est, rowid = est_rowid, phi = phi)
}


get_unconditional_scalar_comparison_phi <- function(group, hi, lo, y, rowid, n, theta) {
    idx <- group$idx
    ng <- length(idx)
    out <- numeric(n)
    if (ng <= 1) {
        return(out)
    }

    for (j in seq_along(idx)) {
        keep <- seq_along(idx) != j
        args <- group$args
        args <- lapply(args, function(x) {
            if (length(x) == ng) {
                return(x[keep])
            }
            x
        })
        args$hi <- hi[idx][keep]
        args$lo <- lo[idx][keep]
        if (isTRUE(group$uses_y)) {
            args$y <- y[idx][keep]
        }
        theta_minus <- do_call(group$fun, args)
        if (length(theta_minus) != 1L || anyNA(theta_minus)) {
            stop_sprintf(
                "`vcov = \"unconditional\"` could not linearize a scalar comparison function."
            )
        }
        out[rowid[idx[j]]] <- out[rowid[idx[j]]] +
            (n / ng) * (ng - 1) * (theta - theta_minus)
    }
    out
}


get_unconditional_aggregate_phi <- function(agg, base_est, rowid, n, base_phi = NULL) {
    if (is.null(base_phi)) {
        base_phi <- matrix(0, nrow = n, ncol = length(base_est))
    }

    if (is.null(agg)) {
        return(base_phi)
    }

    out <- matrix(0, nrow = n, ncol = agg$n)

    for (block in agg$blocks) {
        idx_mat <- block$idx
        e <- base_est[idx_mat]
        dim(e) <- dim(idx_mat)
        rid <- rowid[idx_mat]
        dim(rid) <- dim(idx_mat)

        if (isTRUE(agg$weighted)) {
            w <- block$w
        } else {
            w <- matrix(1, nrow = nrow(idx_mat), ncol = ncol(idx_mat))
        }

        for (j in seq_along(block$cols)) {
            col <- block$cols[[j]]
            idx <- idx_mat[, j]
            ej <- e[, j]
            rj <- rid[, j]
            wj <- w[, j]
            ok <- !is.na(ej) & !is.na(wj) & wj != 0
            if (!any(ok)) {
                next
            }
            theta <- sum(ej[ok] * wj[ok]) / sum(wj[ok])
            alpha <- numeric(length(ej))
            alpha[ok] <- wj[ok] / sum(wj[ok])
            out[, col] <- out[, col] + as.vector(base_phi[, idx, drop = FALSE] %*% alpha)

            ok_rowid <- ok & !is.na(rj)
            if (any(ok_rowid)) {
                contrib <- numeric(length(ej))
                contrib[ok_rowid] <- n * wj[ok_rowid] / sum(wj[ok]) * (ej[ok_rowid] - theta)
                out[, col] <- out[, col] + rowsum_unconditional(contrib[ok_rowid], rj[ok_rowid], n)
            }
        }
    }

    out[is.na(out)] <- 0
    out
}


apply_unconditional_hypothesis_phi <- function(phi, plan, pre_hypothesis_estimate) {
    if (is.null(plan$hyp)) {
        return(phi)
    }

    if (is.null(plan$agg)) {
        theta <- pre_hypothesis_estimate
    } else {
        theta <- apply_plan_aggregation(plan$agg, pre_hypothesis_estimate)
    }

    H <- get_jacobian(
        func = plan$hyp$apply,
        x = theta,
        numderiv = list("fdforward")
    )
    phi %*% t(H)
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


get_unconditional_source_rowid <- function(x, n, modeldata = NULL, allow_mismatch = character()) {
    if ("rowidcf" %in% colnames(x)) {
        rowidcf <- normalize_unconditional_rowidcf(
            x[["rowidcf"]],
            n,
            allow_subset = length(allow_mismatch) > 0)
        if (!anyNA(rowidcf) && all(rowidcf >= 1L & rowidcf <= n)) {
            matches <- is.null(modeldata) ||
                isTRUE(unconditional_source_matches_modeldata(
                    x,
                    modeldata,
                    rowidcf,
                    allow_mismatch = allow_mismatch))
            if (isTRUE(matches)) {
                return(rowidcf)
            }
        }
    }

    rowid <- x[["rowid"]]
    if (!is.null(rowid)) {
        rowid <- as.integer(rowid)
        if (
            !anyNA(rowid) &&
                all(rowid %in% seq_len(n)) &&
                (
                    is.null(modeldata) ||
                        isTRUE(unconditional_source_matches_modeldata(
                            x,
                            modeldata,
                            rowid,
                            allow_mismatch = allow_mismatch))
                )
        ) {
            return(rowid)
        }
    }

    if (!is.null(modeldata)) {
        matched <- match_unconditional_source_modeldata(
            x,
            modeldata,
            allow_mismatch = allow_mismatch)
        if (!is.null(matched)) {
            return(matched)
        }
        return(NULL)
    }

    rowid
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


normalize_unconditional_rowidcf <- function(rowidcf, n, allow_subset = FALSE) {
    rowidcf <- as.integer(rowidcf)
    if (anyNA(rowidcf) || any(rowidcf < 1L)) {
        return(rowidcf)
    }
    if (all(rowidcf <= n)) {
        if (setequal(unique(rowidcf), seq_len(n))) {
            return(rowidcf)
        }
        if (isTRUE(allow_subset)) {
            return(rowidcf)
        }
        return(rep(NA_integer_, length(rowidcf)))
    }

    max_id <- max(rowidcf)
    if (
        max_id %% n == 0L &&
            setequal(unique(rowidcf), seq_len(max_id))
    ) {
        return(((rowidcf - 1L) %% n) + 1L)
    }

    rowidcf
}


rowsum_unconditional <- function(x, group, n) {
    out <- numeric(n)
    s <- rowsum(x, group, reorder = FALSE)
    idx <- as.integer(rownames(s))
    out[idx] <- s[, 1]
    out
}


sanitize_unconditional_rowid <- function(rowid, n) {
    if (is.null(rowid)) {
        stop_sprintf("`vcov = \"unconditional\"` requires `rowid` in the marginaleffects evaluation data.")
    }
    rowid <- as.integer(rowid)
    if (anyNA(rowid) || any(rowid < 1L) || any(rowid > n)) {
        stop_sprintf(
            "`vcov = \"unconditional\"` currently requires evaluation rows to map to the original model data."
        )
    }
    rowid
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


get_unconditional_coef <- function(model) {
    beta <- stats::coef(model)
    if (anyNA(beta)) {
        stop_sprintf("`vcov = \"unconditional\"` does not support models with aliased coefficients.")
    }
    beta
}


get_unconditional_beta_dot <- function(model) {
    insight::check_if_installed("sandwich")
    if (inherits(model, "fixest")) {
        insight::check_if_installed("fixest")
        scores <- model$scores
        bread <- tryCatch(
            fixest::bread(model),
            error = function(e) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires model scores and a ",
                    "bread matrix. `fixest::bread()` failed for this model: %s"
                )
                stop_sprintf(
                    msg,
                    conditionMessage(e)
                )
            }
        )
    } else {
        scores <- tryCatch(
            sandwich::estfun(model),
            error = function(e) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires a ",
                    "`sandwich::estfun()` method for models of class \"%s\". ",
                    "Original error: %s"
                )
                stop_sprintf(
                    msg,
                    class(model)[1],
                    conditionMessage(e)
                )
            }
        )
        bread <- tryCatch(
            sandwich::bread(model),
            error = function(e) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires a ",
                    "`sandwich::bread()` method for models of class \"%s\". ",
                    "Original error: %s"
                )
                stop_sprintf(
                    msg,
                    class(model)[1],
                    conditionMessage(e)
                )
            }
        )
    }

    beta <- get_coef(model)
    if (anyNA(beta)) {
        stop_sprintf("`vcov = \"unconditional\"` does not support models with aliased coefficients.")
    }
    if (is.null(names(beta)) || anyNA(names(beta)) || any(names(beta) == "")) {
        stop_sprintf("`vcov = \"unconditional\"` requires named model coefficients.")
    }
    if (!isTRUE(checkmate::check_matrix(scores, min.rows = 1, min.cols = 1))) {
        stop_sprintf(
            "`vcov = \"unconditional\"` could not extract a valid score matrix for this model."
        )
    }
    if (!isTRUE(checkmate::check_matrix(bread, nrows = ncol(scores), ncols = ncol(scores)))) {
        stop_sprintf(
            "`vcov = \"unconditional\"` could not extract a valid bread matrix for this model."
        )
    }
    if (is.null(colnames(scores)) && ncol(scores) == length(beta)) {
        colnames(scores) <- names(beta)
    }
    if (is.null(colnames(bread)) && ncol(bread) == length(beta)) {
        colnames(bread) <- names(beta)
    }
    if (is.null(rownames(bread)) && nrow(bread) == length(beta)) {
        rownames(bread) <- names(beta)
    }

    cols <- names(beta)
    missing_scores <- setdiff(cols, colnames(scores))
    missing_bread_rows <- setdiff(cols, rownames(bread))
    missing_bread_cols <- setdiff(cols, colnames(bread))
    if (length(missing_scores) > 0 || length(missing_bread_rows) > 0 || length(missing_bread_cols) > 0) {
        missing <- unique(c(missing_scores, missing_bread_rows, missing_bread_cols))
        msg <- paste0(
            "`vcov = \"unconditional\"` could not align model coefficients ",
            "with the score and bread matrices. Missing columns: %s."
        )
        stop_sprintf(msg, paste(missing, collapse = ", "))
    }

    scores <- scores[, cols, drop = FALSE]
    bread <- bread[cols, cols, drop = FALSE]
    scores %*% bread
}


get_unconditional_vcov <- function(Phi, inputs) {
    n <- inputs$n
    correction <- get_unconditional_correction(inputs)
    if (inputs$vcov$type == "cluster") {
        S <- rowsum(Phi, inputs$vcov$cluster, reorder = FALSE)
        return(crossprod(S) / n^2 * correction)
    }
    crossprod(Phi) / n^2 * correction
}


get_unconditional_correction <- function(inputs) {
    if (inputs$vcov$type == "HC0") {
        return(1)
    }

    model <- inputs$model
    n <- inputs$n
    k <- length(get_unconditional_coef(model))
    is_glm <- inherits(model, "glm")
    is_fixest <- inherits(model, "fixest")
    is_feols <- is_fixest && identical(model$method_type, "feols")
    is_linear <- (inherits(model, "lm") && !is_glm) || is_feols

    if (inputs$vcov$type == "cluster") {
        G <- length(unique(inputs$vcov$cluster))
        if (G < 2) {
            stop_sprintf("Cluster-robust unconditional variance requires at least two clusters.")
        }
        if (isTRUE(is_linear)) {
            if (n <= k) {
                msg <- paste0(
                    "`vcov = \"unconditional\"` requires more observations ",
                    "than estimated coefficients for finite-sample corrected ",
                    "linear inference. Use `unconditional(\"HC0\", df = Inf)` ",
                    "to omit the finite-sample correction."
                )
                stop_sprintf(msg)
            }
            return((G / (G - 1)) * ((n - 1) / (n - k)))
        }
        return(G / (G - 1))
    }

    if (isTRUE(is_linear)) {
        if (n <= k) {
            msg <- paste0(
                "`vcov = \"unconditional\"` requires more observations ",
                "than estimated coefficients for finite-sample corrected ",
                "linear inference. Use `unconditional(\"HC0\", df = Inf)` ",
                "to omit the finite-sample correction."
            )
            stop_sprintf(msg)
        }
        return(n / (n - k))
    }
    if (n <= 1) {
        msg <- paste0(
            "`vcov = \"unconditional\"` requires at least two observations ",
            "for finite-sample corrected inference. Use ",
            "`unconditional(\"HC0\", df = Inf)` to omit the finite-sample ",
            "correction."
        )
        stop_sprintf(msg)
    }
    n / (n - 1)
}
