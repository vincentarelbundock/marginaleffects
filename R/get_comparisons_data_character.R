get_comparisons_data_character <- function(
    model,
    newdata,
    variable,
    cross,
    first_cross,
    modeldata,
    ...
) {
    # factors store all levels, but characters do not, so we need to extract the
    # original data from the model.
    tmp <- modeldata

    # unsupported by insight (e.g., numpyro)
    if (is.null(tmp)) {
        tmp <- newdata
    }

    levs <- sort(unique(tmp[[variable$name]]))

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
            isTRUE(checkmate::check_data_frame(variable$value)) ||
            isTRUE(checkmate::check_data_table(variable$value))
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
            levs_idx <- data.table::data.table(
                lo = as.character(variable$value[1]),
                hi = as.character(variable$value[2])
            )
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

    lo[[variable$name]] <- lo[["marginaleffects_contrast_lo"]]
    hi[[variable$name]] <- hi[["marginaleffects_contrast_hi"]]
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
