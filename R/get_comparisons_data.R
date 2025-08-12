get_comparisons_data <- function(
    mfx,
    variables,
    cross,
    ...
) {
    newdata <- mfx@newdata
    model <- mfx@model
    modeldata <- mfx@modeldata

    lo <- hi <- ter <- lab <- original <- rowid <- list()

    # after variable class assignment
    if (is.null(modeldata)) {
        modeldata <- attr(newdata, "newdata_modeldata")
    }
    # sometimes needed for extensions when get_data doesn't work
    if (is.null(modeldata) || nrow(modeldata) == 0) {
        modeldata <- newdata
    }

    # safety need for extensions not supported by `insight`
    variable_classes <- attr(newdata, "newdata_variable_class")
    if (length(variable_classes) == 0) {
        newdata <- set_variable_class(newdata, model)
        variable_classes <- attr(newdata, "marginaleffects_variable_class")
    }
    if (length(attr(modeldata, "marginaleffects_variable_class")) == 0) {
        modeldata <- set_variable_class(modeldata, model)
    }

    if (any(c("factor", "character") %in% variable_classes)) {
        first_cross <- names(variable_classes[
            variable_classes %in% c("factor", "character")
        ])[1]
    } else {
        first_cross <- NULL
    }

    # must use `as.data.table()` because `setDT()` does not handle columns with
    # more dimensions (e.g., "idx" in {mlogit})
    newdata <- as.data.table(newdata)

    for (v in variables) {
        args <- list(
            model = model,
            newdata = newdata,
            variable = v,
            cross = cross,
            first_cross = identical(v$name, first_cross),
            modeldata = modeldata
        )
        args <- append(args, list(...))

        # logical and character before factor used to be important; but I don't think so anymore
        if (get_variable_class(modeldata, v$name, "logical")) {
            fun <- get_comparisons_data_logical
        } else if (get_variable_class(modeldata, v$name, "character")) {
            fun <- get_comparisons_data_character
        } else if (get_variable_class(modeldata, v$name, "categorical")) {
            fun <- get_comparisons_data_factor
        } else if (get_variable_class(modeldata, v$name, "numeric")) {
            fun <- get_comparisons_data_numeric
        } else {
            msg <- sprintf(
                "Class of the `%s` variable is class is not supported.",
                v$name
            )
            stop(msg, call. = FALSE)
        }

        tmp <- do.call("fun", args)

        lo[[v$name]] <- tmp$lo
        if (isTRUE(cross)) {
            lo[[v$name]][[paste0("null_contrast_", v$name)]] <- tmp$contrast_null
        }
        hi[[v$name]] <- tmp$hi
        ter[[v$name]] <- tmp$ter
        lab[[v$name]] <- tmp$lab
        original[[v$name]] <- tmp$original
        rowid[[v$name]] <- tmp$rowid
    }

    clean <- function(x) {
        for (col in colnames(x)) {
            # tobit1 introduces AsIs columns
            if (inherits(x[[col]], "AsIs")) {
                x[[col]] <- as.numeric(x[[col]])
            }

            # plm creates c("pseries", "numeric"), but when get_comparisons_data
            # assigns +1 numeric, we lose the inheritance
            if (inherits(x[[col]], "pseries")) {
                x[[col]] <- as.numeric(x[[col]])
            }

            # strip labelled data which break rbindlist()
            cl <- class(x[[col]])
            if (length(cl) == 2 && cl[1] == "labelled") {
                class(x[[col]]) <- class(x[[col]])[2]
            }
        }
        return(x)
    }

    lo <- lapply(lo, clean)
    hi <- lapply(hi, clean)
    original <- lapply(original, clean)

    # single contrast
    if (!isTRUE(cross)) {
        lo <- rbindlist(lo, fill = TRUE, ignore.attr = TRUE)
        hi <- rbindlist(hi, fill = TRUE, ignore.attr = TRUE)
        original <- rbindlist(original, fill = TRUE, ignore.attr = TRUE)
        # long names to avoid user-supplied colname conflict
        marginaleffects_ter <- unlist(ter, use.names = FALSE)
        marginaleffects_lab <- unlist(lab, use.names = FALSE)
        lo[, "term" := marginaleffects_ter]
        hi[, "term" := marginaleffects_ter]
        original[, "term" := marginaleffects_ter]
        lo[, "contrast" := marginaleffects_lab]
        hi[, "contrast" := marginaleffects_lab]
        original[, "contrast" := marginaleffects_lab]

        # cross contrast
    } else {
        # drop variables for which we have contrasts
        for (i in seq_along(lo)) {
            if (i == 1) {
                # keep rowid and original data only in one of the datasets
                idx_lo <- setdiff(names(variables), names(lo)[i])
                idx_hi <- setdiff(names(variables), names(hi)[i])
                idx_or <- setdiff(names(variables), names(hi)[i])
            } else {
                # exclude rowid and variables excluded from `variables`, for
                # which we do not compute cross-contrasts
                contrast_null <- grep(
                    "rowid|^null_contrast_",
                    colnames(lo[[i]]),
                    value = TRUE
                )
                idx_lo <- c(
                    setdiff(names(lo[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(lo)[[i]])
                )
                idx_hi <- c(
                    setdiff(names(hi[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(hi)[[i]])
                )

                idx_or <- c(
                    setdiff(names(original[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(original)[[i]])
                )
            }
            lo[[i]] <- data.table(lo[[i]])[, !..idx_lo]
            hi[[i]] <- data.table(hi[[i]])[, !..idx_hi]
            original[[i]] <- data.table(original[[i]])[, !..idx_or]
            lo[[i]][[paste0("contrast_", names(lo)[i])]] <- lab[[i]]
            hi[[i]][[paste0("contrast_", names(hi)[i])]] <- lab[[i]]
            original[[i]][[paste0("contrast_", names(original)[i])]] <- lab[[i]]
        }

        fun <- function(x, y) merge(x, y, all = TRUE, allow.cartesian = TRUE, sort = FALSE)
        lo <- Reduce("fun", lo)
        hi <- Reduce("fun", hi)
        original <- Reduce("fun", original)

        # faster to rbind, but creates massive datasets. need cartesian join on rowid
        # lo <- cjdt(lo)
        # hi <- cjdt(hi)

        # if there are fewer null_contrast_* columns, then there is at least
        # one always non-null variable type, so we keep everything
        idx <- grepl("^null_contrast_", colnames(lo))
        idx_df <- lo[, ..idx]
        lo <- lo[, !..idx]
        if (sum(idx) == length(variables)) {
            idx <- rowSums(idx_df) < ncol(idx_df)
            lo <- lo[idx]
            hi <- hi[idx]
            original <- original[idx]
        }
    }

    # get_predict() is much faster if we only build the model matrix once
    lo <- add_model_matrix_attribute(mfx, lo)
    hi <- add_model_matrix_attribute(mfx, hi)
    original <- add_model_matrix_attribute(mfx, original)

    out <- list(lo = lo, hi = hi, original = original)

    return(out)
}
