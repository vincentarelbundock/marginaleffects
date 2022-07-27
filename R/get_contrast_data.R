get_contrast_data <- function(model,
                              newdata,
                              variables,
                              interaction,
                              eps,
                              ...) {

    lo <- hi <- ter <- lab <- original <- rowid <- list()

    modeldata <- attr(newdata, "newdata_modeldata")
    variable_classes <- attr(newdata, "newdata_variable_class")

    if (any(c("factor", "character") %in% variable_classes)) {
        first_interaction <- names(variable_classes[variable_classes %in% c("factor", "character")])[1]
    } else {
        first_interaction <- NULL
    }

    for (v in variables) {
        args <- list(
            model = model,
            newdata = newdata,
            variable = v,
            interaction = interaction,
            first_interaction = identical(v$name, first_interaction))
        args <- append(args, list(...))
        if (is.null(eps) && variable_classes[[v$name]] == "numeric") {
            args[["eps"]] <- 1e-4 * diff(range(modeldata[[v$name]], na.rm = TRUE))
        } else {
            args[["eps"]] <- eps
        }

        # logical and character before factor because they get picked up by find_variable_class()
        if (identical(variable_classes[[v$name]], "logical")) {
            fun <- get_contrast_data_logical
        } else if (identical(variable_classes[[v$name]], "character")) {
            fun <- get_contrast_data_character
        } else if (identical(variable_classes[[v$name]], "factor")) {
            fun <- get_contrast_data_factor
        } else if (identical(variable_classes[[v$name]], "numeric")) {
            fun <- get_contrast_data_numeric
        } else {
            msg <- sprintf("Class of the `%s` variable is class is not supported.", v)
            stop(msg, call. = FALSE)
        }

        tmp <- do.call("fun", args)

        lo[[v$name]] <- tmp$lo
        if (isTRUE(interaction)) {
            lo[[v$name]][[paste0("null_contrast_", v$name)]] <- tmp$contrast_null
        }
        hi[[v$name]] <- tmp$hi
        ter[[v$name]] <- tmp$ter
        lab[[v$name]] <- tmp$lab
        original[[v$name]] <- tmp$original
        rowid[[v$name]] <- tmp$rowid
    }

    # clean before merge: tobit1 introduces AsIs columns
    clean <- function(x) {
        for (col in colnames(x)) {
            if (inherits(x[[col]], "AsIs")) {
                x[[col]] <- as.numeric(x[[col]])
             }
            # mlogit uses a `newdata` with one row per unit-choice and returns
            # an `idx` column with the choice label in second position
            if (inherits(model, "mlogit") && inherits(x[[col]], "idx")) {
                x[[col]] <- NULL
            }
        }
        return(x)
    }


    lo <- lapply(lo, clean)
    hi <- lapply(hi, clean)
    original <- lapply(original, clean)


    # single contrast
    if (!isTRUE(interaction)) {
        lo <- rbindlist(lo, fill = TRUE)
        hi <- rbindlist(hi, fill = TRUE)
        original <- rbindlist(original, fill = TRUE)
        ter <- unlist(ter)
        lab <- unlist(lab)
        lo[, "term" := ter]
        hi[, "term" := ter]
        original[, "term" := ter]
        lo[, "contrast" := lab]
        hi[, "contrast" := lab]
        original[, "contrast" := lab]

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
                contrast_null <- grep("rowid|^null_contrast_", colnames(lo[[i]]), value = TRUE)
                idx_lo <- c(setdiff(names(lo[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(lo)[[i]]))
                idx_hi <- c(setdiff(names(hi[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(hi)[[i]]))

                idx_or <- c(setdiff(names(original[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(original)[[i]]))
            }
            lo[[i]] <- data.table(lo[[i]])[, !..idx_lo]
            hi[[i]] <- data.table(hi[[i]])[, !..idx_hi]
            original[[i]] <- data.table(original[[i]])[, !..idx_or]
            lo[[i]][[paste0("contrast_", names(lo)[i])]] <- lab[[i]]
            hi[[i]][[paste0("contrast_", names(hi)[i])]] <- lab[[i]]
            original[[i]][[paste0("contrast_", names(original)[i])]] <- lab[[i]]
        }

        fun <- function(x, y) merge(x, y, all = TRUE, allow.cartesian = TRUE)
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

    out <- list(lo = lo, hi = hi, original = original)

    return(out)
}
