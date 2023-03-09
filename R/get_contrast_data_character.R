get_contrast_data_character <- function(model,
                                        newdata,
                                        variable,
                                        cross,
                                        first_cross,
                                        modeldata = NULL,
                                        ...) {

    # factors store all levels, but characters do not, so we need to extract the
    # original data from the model.
    if (is.null(modeldata)) {
        tmp <- modeldata <- get_modeldata(model, additional_variables = FALSE)
    } else {
        tmp <- modeldata
    }

    # unsupported by insight (e.g., numpyro)
    if (is.null(tmp)) {
        tmp <- newdata
    }

    levs <- sort(unique(tmp[[variable$name]]))

    # index contrast orders based on variable$value
    if (isTRUE(variable$value == "reference")) {
        # null contrasts are interesting with interactions
        if (!isTRUE(interaction)) {
            levs_idx <- data.table::data.table(lo = levs[1], hi = levs[2:length(levs)])
        } else {
            levs_idx <- data.table::data.table(lo = levs[1], hi = levs)
        }

    } else if (isTRUE(variable$value == "pairwise")) {
        levs_idx <- CJ(lo = levs, hi = levs, sorted = FALSE)
        # null contrasts are interesting with interactions
        if (!isTRUE(interaction)) {
            levs_idx <- levs_idx[levs_idx$hi != levs_idx$lo, ]
            levs_idx <- levs_idx[match(levs_idx$lo, levs) < match(levs_idx$hi, levs), ]
        }

    } else if (isTRUE(variable$value == "all")) {
        levs_idx <- CJ(lo = levs, hi = levs, sorted = FALSE)

    } else if (isTRUE(variable$value == "sequential")) {
        levs_idx <- data.table::data.table(lo = levs[1:(length(levs) - 1)],
                                           hi = levs[2:length(levs)])

    } else if (length(variable$value) == 2) {
        if (is.character(variable$value)) {
            tmp <- modeldata[[variable$name]]
            if (any(!variable$value %in% as.character(tmp))) {
                msg <- "Some of the values supplied to the `variables` argument were not found in the dataset."
                insight::format_error(msg)
            }
            idx <- match(variable$value, as.character(tmp))
            levs_idx <- data.table::data.table(lo = tmp[idx[1]], hi = tmp[idx[[2]]])
        } else if (is.numeric(variable$value)) {
            tmp <- newdata[[variable$name]]
            levs_idx <- data.table::data.table(
                lo = as.character(variable$value[1]),
                hi = as.character(variable$value[2]))
        } else {
            levs_idx <- data.table::data.table(lo = variable$value[1], hi = variable$value[2])
        }
    }

    # internal option applied to the first of several contrasts when
    # interaction=TRUE to avoid duplication. when only the first contrast
    # flips, we get a negative sign, but if first increases and second
    # decreases, we get different total effects.
    if (isTRUE(first_cross)) {
        idx <- match(levs_idx$hi, levs) >= match(levs_idx$lo, levs)
        if (sum(idx) > 0) {
            levs_idx <- levs_idx[idx, , drop = FALSE]
        }
    }

    levs_idx$isNULL <- levs_idx$hi == levs_idx$lo
    levs_idx$label <- suppressWarnings(tryCatch(
        sprintf(variable$label, levs_idx$hi, levs_idx$lo),
        error = function(e) variable$label))
    levs_idx <- stats::setNames(levs_idx, paste0("marginaleffects_contrast_", colnames(levs_idx)))
    if (!"marginaleffects_contrast_label" %in% colnames(levs_idx) || all(levs_idx$marginaleffects_contrast_label == "custom")) {
        levs_idx[, "marginaleffects_contrast_label" := paste0(marginaleffects_contrast_hi, ", ", marginaleffects_contrast_lo)]
    }

    lo <- hi <- cjdt(list(newdata, levs_idx))

    lo[[variable$name]] <- lo[["marginaleffects_contrast_lo"]]
    hi[[variable$name]] <- hi[["marginaleffects_contrast_hi"]]
    contrast_label <- hi$marginaleffects_contrast_label
    contrast_null <- hi$marginaleffects_contrast_hi == hi$marginaleffects_contrast_lo

    tmp <- !grepl("^marginaleffects_contrast", colnames(lo))
    lo <- lo[, tmp, with = FALSE]
    hi <- hi[, tmp, with = FALSE]

    original <- data.table::rbindlist(rep(list(newdata), nrow(levs_idx)))

    out <- list(rowid = original$rowid,
                lo = lo,
                hi = hi,
                original = original,
                ter = rep(variable$name, nrow(lo)), # lo can be different dimension than newdata
                lab = contrast_label,
                contrast_null = contrast_null)
    return(out)
}
