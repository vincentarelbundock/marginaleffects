
    # should we try to compute predictions with `insight::get_predicted()`?
    # confidence interval with known `predict` argument
    is_insight <- (!isFALSE(vcov) && !is.na(type_insight)) ||
        any(c("include_random", "include_smooth") %in% names(dots))

    # `insight::get_predicted` yields back-transformed confidence intervals
    if (isTRUE(is_insight)) {
        if ("re_formula" %in% names(dots)) {
            dots[["re.form"]] <- dots[["re_formula"]]
        }

        if ("re.form" %in% names(dots)) {
            if (isTRUE(checkmate::check_formula(dots[["re.form"]]))) {
                dots[["include_random"]] <- dots[["re.form"]]
            } else if (is.na(dots[["re.form"]])) {
                dots[["include_random"]] <- FALSE
            } else {
                dots[["include_random"]] <- TRUE
            }
            dots[["re.form"]] <- NULL
        }

        args <- list(
            x = model,
            data = newdata,
            predict = type_insight,
            ci = conf_level)

        # `get_predicted` issues a warning even with `vcov=NULL` when the
        # argument is not supported, so we do this here instead of in `predictions`
        if (isTRUE(vcov == "satterthwaite") || isTRUE(vcov == "kenward-roger")) {
            args[["ci_method"]] <- vcov
            # lmerTest predict method fails when the DV is not there
            dv <- insight::find_response(model)
            newdata_tmp <- newdata
            newdata_tmp[[dv]] <- mean(insight::get_response(model))
            args[["data"]] <- newdata_tmp
        } else if (is.logical(vcov)) {
            args[["vcov"]] <- NULL
        } else if (!is.logical(vcov) && !is.null(vcov)) {
            args[["vcov"]] <- get_vcov(model, vcov = vcov)
        }

        args <- c(args, dots)

        fun <- insight::get_predicted
        pred <- try(do.call("fun", args), silent = TRUE)

        # return immediately if this worked
        if (inherits(pred, "get_predicted")) {
            out <- data.frame(pred) # cannot use data.table because insight has no as.data.table method
            if ("rowid" %in% colnames(out)) {
                out[["Row"]] <- NULL
            } else {
                colnames(out)[colnames(out) == "Row"] <- "rowid"
            }
            colnames(out)[colnames(out) == "Response"] <- "group"
            colnames(out)[colnames(out) == "SE"] <- "std.error"
            colnames(out)[colnames(out) == "Predicted"] <- "estimate"
            colnames(out)[colnames(out) == "CI_low"] <- "conf.low"
            colnames(out)[colnames(out) == "CI_high"] <- "conf.high"

            if (nrow(out) == nrow(newdata) && "rowid" %in% colnames(newdata)) {
                out$rowid <- newdata$rowid
            }

            out <- sort_columns(out, first = c("rowid", "group", "estimate"))
            return(out)
        }
    }
