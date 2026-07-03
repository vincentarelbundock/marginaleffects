prediction_plan_build <- function(
    mfx,
    type,
    model_perturbed = NULL,
    by = NULL,
    byfun = NULL,
    hypothesis = NULL,
    verbose = TRUE,
    hi = NULL,
    lo = NULL,
    original = NULL,
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
            "rowid" %in% colnames(mfx@newdata) &&
            nrow(out) == nrow(mfx@newdata)
    ) {
        out$rowid <- mfx@newdata$rowid
    }

    draws <- attr(out, "posterior_draws")
    raw_estimate <- out[["estimate"]]

    cols <- setdiff(colnames(newdata), colnames(out))
    if (inherits(mfx@model, "mlogit")) {
        out <- merge_by_rowid(out, newdata)
    } else {
        nd <- subset(newdata, select = cols)
        out <- cbind(out, nd)
    }

    keep <- NULL
    if ("rowid" %in% colnames(out)) {
        idx_keep <- out$rowid > 0
        if (!all(idx_keep)) {
            keep <- which(idx_keep)
        }
    }

    tmp <- unpad(out, draws)
    list2env(tmp, envir = environment())

    if (isTRUE(checkmate::check_character(by))) {
        by <- intersect(c("group", by), colnames(out))
    }

    plan <- NULL
    if (is.null(draws)) {
        agg <- estimate_plan_record_agg(
            out,
            newdata = newdata,
            by = by,
            byfun = byfun,
            verbose = verbose,
            ...
        )
        out <- agg$out
        hyp <- hypothesis_compile(
            hypothesis,
            out,
            by = by,
            newdata = newdata,
            mfx = mfx
        )
        out <- hyp$cmp
        plan <- list(
            n_pred = length(raw_estimate),
            predict_args = list(
                type = type,
                newdata = newdata,
                mfx = mfx,
                dots = dots
            ),
            keep = keep,
            agg = agg$agg,
            hyp = hyp$hyp,
            check = list(n_pred = length(raw_estimate))
        )
        baseline <- prediction_plan_apply(plan, raw_estimate)
        if (!isTRUE(all.equal(baseline, out[["estimate"]], tolerance = 1e-12, check.attributes = FALSE))) {
            stop_sprintf("Internal error: prediction plan baseline check failed.")
        }
    } else {
        out <- get_by(
            out,
            draws = draws,
            newdata = newdata,
            by = by,
            byfun = byfun,
            verbose = verbose,
            ...
        )
        draws <- attr(out, "posterior_draws")
        out <- get_hypothesis(
            out,
            hypothesis = hypothesis,
            by = by,
            newdata = newdata,
            draws = draws,
            mfx = mfx
        )
    }

    list(cmp = out, plan = plan)
}

prediction_plan_apply <- function(plan, pred) {
    stopifnot(length(pred) == plan$check$n_pred)
    if (!is.null(plan$keep)) {
        pred <- pred[plan$keep]
    }
    if (!is.null(plan$agg)) {
        pred <- estimate_plan_apply_agg(plan$agg, pred)
    }
    if (!is.null(plan$hyp)) {
        pred <- plan$hyp$apply(pred)
    }
    pred
}

prediction_plan_predict <- function(.plan, model_perturbed, ...) {
    dots <- estimate_plan_predict_dots(.plan$predict_args$dots, list(...))
    args <- c(
        list(
            model = model_perturbed,
            newdata = .plan$predict_args$newdata,
            type = .plan$predict_args$type,
            mfx = .plan$predict_args$mfx
        ),
        dots
    )
    pred <- do_call(get_predict, args)
    pred[["estimate"]]
}
