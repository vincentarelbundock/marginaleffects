get_contrast_data_character <- function(model,
                                        newdata,
                                        variable,
                                        interaction,
                                        first_interaction,
                                        ...) {

    # factors store all levels, but characters do not, so we need to extract the
    # original data from the model.
    tmp <- hush(insight::get_data(model))
    levs <- sort(unique(tmp[[variable$name]]))

    # index contrast orders based on variable$value
    if (variable$value == "reference") {
        # null contrasts are interesting with interactions
        if (!isTRUE(interaction)) {
            levs_idx <- data.table::data.table(lo = levs[1], hi = levs[2:length(levs)])
        } else {
            levs_idx <- data.table::data.table(lo = levs[1], hi = levs)
        }

    } else if (variable$value == "pairwise") {
        levs_idx <- CJ(lo = levs, hi = levs, sorted = FALSE)
        # null contrasts are interesting with interactions
        if (!isTRUE(interaction)) {
            levs_idx <- levs_idx[levs_idx$hi != levs_idx$lo, ]
            levs_idx <- levs_idx[match(levs_idx$lo, levs) < match(levs_idx$hi, levs), ]
        }

    } else if (variable$value == "all") {
        levs_idx <- CJ(lo = levs, hi = levs, sorted = FALSE)

    } else if (variable$value == "sequential") {
        levs_idx <- data.table::data.table(lo = levs[1:(length(levs) - 1)],
                                           hi = levs[2:length(levs)])
    }

    # internal option applied to the first of several contrasts when
    # interaction=TRUE to avoid duplication. when only the first contrast
    # flips, we get a negative sign, but if first increases and second
    # decreases, we get different total effects.
    if (isTRUE(first_interaction)) {
        levs_idx <- levs_idx[match(levs_idx$hi, levs) >= match(levs_idx$lo, levs), ]
    }

    levs_idx$isNULL <- levs_idx$hi == levs_idx$lo
    levs_idx$label <- suppressWarnings(tryCatch(
        sprintf(variable$label, levs_idx$hi, levs_idx$lo),
        error = function(e) variable$label))
    levs_idx <- stats::setNames(levs_idx, paste0("marginaleffects_contrast_", colnames(levs_idx)))

    setDT(newdata)
    lo <- hi <- cjdt(list(newdata, levs_idx))

    lo[[variable$name]] <- lo[["marginaleffects_contrast_lo"]]
    hi[[variable$name]] <- hi[["marginaleffects_contrast_hi"]]
    contrast_label <- hi$marginaleffects_contrast_label
    contrast_null <- hi$marginaleffects_contrast_hi == hi$marginaleffects_contrast_lo

    idx <- grepl("^marginaleffects_contrast", colnames(lo))
    lo <- lo[, !idx, with = FALSE]
    hi <- hi[, !idx, with = FALSE]

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
