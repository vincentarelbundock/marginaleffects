get_contrast_data <- function(model,
                              newdata,
                              variables,
                              contrast_factor,
                              contrast_numeric,
                              contrast_label,
                              interaction,
                              eps,
                              ...) {

    lo <- hi <- ter <- lab <- original <- rowid <- list()

    originaldata <- hush(insight::get_data(model))

    variable_classes <- sapply(names(variables), function(x) find_variable_class(
                               variable = x,
                               newdata = newdata,
                               model = model))
    if (any(c("factor", "character") %in% variable_classes)) {
        first_interaction <- names(variable_classes[variable_classes %in% c("factor", "character")])[1]
    }
    for (v in names(variables)) {
        # logical and character before factor because they get picked up by find_categorical
        variable_class <- find_variable_class(variable = v,
                                              newdata = newdata,
                                              model = model)

        if (variable_class == "logical") {
            tmp <- get_contrast_data_logical(
                model,
                newdata,
                v,
                contrast_label = variables[[v]]$label,
                ...)

        } else if (variable_class == "factor") {
            tmp <- get_contrast_data_factor(
                model,
                newdata,
                v,
                contrast_factor = variables[[v]]$value,
                interaction = interaction,
                contrast_label = variables[[v]]$label,
                first_interaction = isTRUE(v == first_interaction),
                ...)

        } else if (variable_class == "character") {
            tmp <- get_contrast_data_character(
                model,
                newdata,
                v,
                interaction = interaction,
                contrast_factor = variables[[v]]$value,
                contrast_label = variables[[v]]$label,
                first_interaction = isTRUE(v == first_interaction),
                ...)

        } else if (variable_class == "numeric") {
            if (is.null(eps)) {
                eps_v <- 1e-4 * diff(range(originaldata[[v]], na.rm = TRUE))
            } else {
                eps_v <- eps
            }
            # eps is need twice: to build the contrast data and for post hoc normalization
            tmp <- get_contrast_data_numeric(
                model,
                newdata,
                variables[[v]],
                contrast_label <- variables[[v]]$label,
                eps = eps_v,
                ...)
            tmp[["original"]][["marginaleffects_eps"]] <- eps_v

        } else {
            stop("variable class not supported.", call. = FALSE)
        }

        lo[[v]] <- tmp$lo
        if (isTRUE(interaction)) {
            lo[[v]][[paste0("null_contrast_", v)]] <- tmp$contrast_null
        }
        hi[[v]] <- tmp$hi
        ter[[v]] <- tmp$ter
        lab[[v]] <- tmp$lab
        original[[v]] <- tmp$original
        rowid[[v]] <- tmp$rowid
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
            } else {
                # exclude rowid and variables excluded from `variables`, for
                # which we do not compute cross-contrasts
                contrast_null <- grep("rowid|^null_contrast_", colnames(lo[[i]]), value = TRUE)
                idx_lo <- c(setdiff(names(lo[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(lo)[[i]]))
                idx_hi <- c(setdiff(names(hi[[i]]), c(contrast_null, names(variables))),
                    setdiff(names(variables), names(hi)[[i]]))
            }
            lo[[i]] <- data.table(lo[[i]])[, !..idx_lo]
            hi[[i]] <- data.table(hi[[i]])[, !..idx_hi]
            lo[[i]][[paste0("contrast_", names(lo)[i])]] <- lab[[i]]
            hi[[i]][[paste0("contrast_", names(lo)[i])]] <- lab[[i]]
        }

        fun <- function(x, y) merge(x, y, all = TRUE, allow.cartesian = TRUE)
        lo <- Reduce("fun", lo)
        hi <- Reduce("fun", hi)
        original <- NULL

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
        }
    }

    out <- list(lo = lo, hi = hi, original = original)

    return(out)
}
