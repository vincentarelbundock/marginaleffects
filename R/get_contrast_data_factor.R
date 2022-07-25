get_contrast_data_factor <- function(model,
                                     newdata,
                                     variable,
                                     interaction,
                                     first_interaction,
                                     ...) {

    data.table::setDT(newdata)

    if (is.factor(newdata[[variable$name]])) {
        levs <- levels(newdata[[variable$name]])
        convert_to_factor <- TRUE
    } else {

        msg <- format_msg(
        "The `%s` variable is treated as a categorical (factor) variable, but the
        original data is of class %s. It is safer and faster to convert such variables
        to factor before fitting the model and calling `marginaleffects` functions.")
        msg <- sprintf(msg, variable$name, class(newdata[[variable$name]])[1])
        warn_once(msg, "marginaleffects_warning_factor_on_the_fly_conversion")
        original_data <- hush(insight::get_data(model))
        if (is.factor(original_data[[variable$name]])) {
            levs <- levels(original_data[[variable$name]])
            convert_to_factor <- TRUE
        } else {
            levs <- sort(unique(original_data[[variable$name]]))
            convert_to_factor <- FALSE
        }
    }

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
            levs_idx <- levs_idx[levs_idx$hi != levs_idx$lo,]
            levs_idx <- levs_idx[match(levs_idx$lo, levs) < match(levs_idx$hi, levs),]
        }

    } else if (variable$value == "all") {
        levs_idx <- CJ(lo = levs, hi = levs, sorted = FALSE)

    } else if (variable$value == "sequential") {
        levs_idx <- data.table::data.table(lo = levs[1:(length(levs) - 1)], hi = levs[2:length(levs)])
    }

    # internal option applied to the first of several contrasts when
    # interaction=TRUE to avoid duplication. when only the first contrast
    # flips, we get a negative sign, but if first increases and second
    # decreases, we get different total effects.
    if (isTRUE(first_interaction)) {
        levs_idx <- levs_idx[match(levs_idx$hi, levs) >= match(levs_idx$lo, levs),]
    }

    levs_idx$isNULL <- levs_idx$hi == levs_idx$lo
    levs_idx$label <- suppressWarnings(tryCatch(
        sprintf(variable$label, levs_idx$hi, levs_idx$lo),
        error = function(e) variable$label))
    levs_idx <- stats::setNames(levs_idx, paste0("marginaleffects_contrast_", colnames(levs_idx)))

    lo <- hi <- cjdt(list(newdata, levs_idx))

    original <- data.table::rbindlist(rep(list(newdata), nrow(levs_idx)))

    if (is.factor(newdata[[variable$name]]) || isTRUE(convert_to_factor)) {
        lo[[variable$name]] <- factor(lo[["marginaleffects_contrast_lo"]], levels = levs)
        hi[[variable$name]] <- factor(hi[["marginaleffects_contrast_hi"]], levels = levs)
    } else {
        lo[[variable$name]] <- lo[["marginaleffects_contrast_lo"]]
        hi[[variable$name]] <- hi[["marginaleffects_contrast_hi"]]
    }

    contrast_label <- hi$marginaleffects_contrast_label
    contrast_null <- hi$marginaleffects_contrast_hi == hi$marginaleffects_contrast_lo

    idx <- grepl("^marginaleffects_contrast", colnames(lo))
    lo <- lo[, !idx, with = FALSE]
    hi <- hi[, !idx, with = FALSE]

    out <- list(rowid = original$rowid,
                lo = lo,
                hi = hi,
                original = original,
                ter = rep(variable$name, nrow(lo)), # lo can be different dimension than newdata
                lab = contrast_label,
                contrast_null = contrast_null)
    return(out)
}

