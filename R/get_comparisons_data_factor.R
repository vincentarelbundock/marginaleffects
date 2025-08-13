get_comparisons_data_factor <- function(
    model,
    newdata,
    variable,
    cross,
    first_cross,
    modeldata,
    mfx,
    ...) {
    if (is.factor(newdata[[variable$name]])) {
        levs <- levels(newdata[[variable$name]])
        convert_to_factor <- TRUE
    } else if (get_variable_class(mfx, variable$name, "binary")) {
        levs <- variable$value
        convert_to_factor <- FALSE
    } else {
        if (isTRUE(getOption("marginaleffects_safe", default = TRUE))) {
            msg <- "The `%s` variable is treated as a categorical (factor) variable, but the original data is of class %s. It is safer and faster to convert such variables to factor before fitting the model and calling a `marginaleffects` function."
            msg <- sprintf(msg, variable$name, class(newdata[[variable$name]])[1])
            warn_once(msg, "marginaleffects_warning_factor_on_the_fly_conversion")
        }
        if (is.factor(modeldata[[variable$name]])) {
            levs <- levels(modeldata[[variable$name]])
            convert_to_factor <- TRUE
        } else {
            levs <- sort(unique(modeldata[[variable$name]]))
            convert_to_factor <- FALSE
        }
    }

    # string shortcuts
    flag <- checkmate::check_choice(
        variable$value,
        c(
            "reference",
            "revreference",
            "pairwise",
            "revpairwise",
            "sequential",
            "revsequential",
            "all",
            "minmax"
        )
    )
    if (isTRUE(flag)) {
        levs_idx <- contrast_categories_shortcuts(levs, variable, interaction)

        # custom data frame or function
    } else if (
        isTRUE(checkmate::check_function(variable$value)) ||
            isTRUE(checkmate::check_data_frame(variable$value))
    ) {
        out <- contrast_categories_custom(variable, newdata)
        return(out)

        # vector of two values
    } else if (isTRUE(checkmate::check_atomic_vector(variable$value, len = 2))) {
        if (is.character(variable$value)) {
            tmp <- modeldata[[variable$name]]
            if (!all(variable$value %in% as.character(tmp))) {
                msg <- "Some of the values supplied to the `variables` argument were not found in the dataset."
                stop_sprintf(msg)
            }
            idx <- match(variable$value, as.character(tmp))
            levs_idx <- data.table::data.table(lo = tmp[idx[1]], hi = tmp[idx[[2]]])
        } else if (is.numeric(variable$value)) {
            tmp <- newdata[[variable$name]]
            if (convert_to_factor) {
                levs_idx <- data.table::data.table(
                    lo = factor(as.character(variable$value[1]), levels = levels(tmp)),
                    hi = factor(as.character(variable$value[2]), levels = levels(tmp))
                )
            } else {
                levs_idx <- data.table::data.table(
                    lo = variable$value[1],
                    hi = variable$value[2]
                )
            }
        } else {
            levs_idx <- data.table::data.table(
                lo = variable$value[1],
                hi = variable$value[2]
            )
        }
    }

    tmp <- contrast_categories_processing(
        first_cross,
        levs_idx,
        levs,
        variable,
        newdata
    )
    lo <- tmp[[1]]
    hi <- tmp[[2]]
    original <- tmp[[3]]

    if (is.factor(newdata[[variable$name]]) || isTRUE(convert_to_factor)) {
        lo[[variable$name]] <- factor(
            lo[["marginaleffects_contrast_lo"]],
            levels = levs
        )
        hi[[variable$name]] <- factor(
            hi[["marginaleffects_contrast_hi"]],
            levels = levs
        )
    } else {
        lo[[variable$name]] <- lo[["marginaleffects_contrast_lo"]]
        hi[[variable$name]] <- hi[["marginaleffects_contrast_hi"]]
    }

    contrast_label <- hi$marginaleffects_contrast_label
    contrast_null <- hi$marginaleffects_contrast_hi == hi$marginaleffects_contrast_lo

    tmp <- !grepl("^marginaleffects_contrast", colnames(lo))
    lo <- lo[, tmp, with = FALSE]
    hi <- hi[, tmp, with = FALSE]

    out <- list(
        rowid = original$rowid,
        lo = lo,
        hi = hi,
        original = original,
        ter = rep(variable$name, nrow(lo)), # lo can be different dimension than newdata
        lab = contrast_label,
        contrast_null = contrast_null
    )
    return(out)
}


contrast_categories_shortcuts <- function(levs, variable, interaction) {
    # index contrast orders based on variable$value
    if (isTRUE(variable$value %in% c("reference", "revreference"))) {
        # null contrasts are interesting with interactions
        if (!isTRUE(interaction)) {
            levs_idx <- data.table::data.table(
                lo = levs[1],
                hi = levs[2:length(levs)]
            )
        } else {
            levs_idx <- data.table::data.table(lo = levs[1], hi = levs)
        }
    } else if (isTRUE(variable$value %in% c("pairwise", "revpairwise"))) {
        levs_idx <- CJ(lo = levs, hi = levs, sorted = FALSE)
        # null contrasts are interesting with interactions
        if (!isTRUE(interaction)) {
            levs_idx <- levs_idx[levs_idx$hi != levs_idx$lo, ]
            levs_idx <- levs_idx[
                match(levs_idx$lo, levs) < match(levs_idx$hi, levs),
            ]
        }
    } else if (isTRUE(variable$value %in% c("sequential", "revsequential"))) {
        levs_idx <- data.table::data.table(
            lo = levs[1:(length(levs) - 1)],
            hi = levs[2:length(levs)]
        )
    } else if (isTRUE(variable$value == "all")) {
        levs_idx <- CJ(lo = levs, hi = levs, sorted = FALSE)
    } else if (isTRUE(variable$value == "minmax")) {
        levs_idx <- data.table::data.table(lo = levs[1], hi = levs[length(levs)])
    }

    if (
        isTRUE(
            variable$value %in% c("revreference", "revpairwise", "revsequential")
        )
    ) {
        levs_idx <- levs_idx[, .(lo = hi, hi = lo)]
    }

    return(levs_idx)
}


contrast_categories_df <- function(variable) {
    # manual data frame
    if (all(c("low", "high") %in% colnames(variable$value))) {
        low <- variable$value$low
        high <- variable$value$high
    } else if (all(c("lo", "hi") %in% colnames(variable$value))) {
        low <- variable$value$low
        high <- variable$value$high
    } else {
        low <- variable$value[[1]]
        high <- variable$value[[2]]
    }
    levs_idx <- data.table::data.table(
        lo = low,
        hi = high
    )
    return(levs_idx)
}


contrast_categories_processing <- function(
    first_cross,
    levs_idx,
    levs,
    variable,
    newdata) {
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
        error = function(e) variable$label
    ))
    levs_idx <- stats::setNames(
        levs_idx,
        paste0("marginaleffects_contrast_", colnames(levs_idx))
    )
    if (
        !"marginaleffects_contrast_label" %in% colnames(levs_idx) ||
            all(levs_idx$marginaleffects_contrast_label == "custom")
    ) {
        levs_idx[
            ,
            "marginaleffects_contrast_label" := paste0(
                marginaleffects_contrast_hi,
                ", ",
                marginaleffects_contrast_lo
            )
        ]
    }

    lo <- hi <- cjdt(list(newdata, levs_idx))
    original <- data.table::rbindlist(rep(list(newdata), nrow(levs_idx)))
    return(list(lo, hi, original))
}


contrast_categories_custom <- function(variable, newdata) {
    original <- newdata
    if (!"rowid" %in% colnames(original)) {
        original$rowid <- seq_len(nrow(original))
    }
    hi <- lo <- original
    if (isTRUE(checkmate::check_function(variable$value))) {
        variables_df <- variable$value(newdata[[variable$name]])
    } else if (isTRUE(checkmate::check_data_frame(variable$value))) {
        variables_df <- variable$value
    }
    checkmate::assert_data_frame(variables_df, nrows = nrow(original))
    if (all(c("low", "high") %in% colnames(variables_df))) {
        lo[[variable$name]] <- variables_df[["low"]]
        hi[[variable$name]] <- variables_df[["high"]]
    } else if (all(c("lo", "hi") %in% colnames(variables_df))) {
        lo[[variable$name]] <- variables_df[["lo"]]
        hi[[variable$name]] <- variables_df[["hi"]]
    } else {
        lo[[variable$name]] <- variables_df[[1]]
        hi[[variable$name]] <- variables_df[[2]]
    }
    out <- list(
        rowid = original$rowid,
        lo = lo,
        hi = hi,
        original = original,
        ter = rep(variable$name, nrow(lo)), # lo can be different dimension than newdata
        lab = "custom",
        contrast_null = rep(FALSE, nrow(lo))
    )
    return(out)
}
