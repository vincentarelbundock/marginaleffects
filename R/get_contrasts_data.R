get_contrast_data <- function(model,
                              newdata = NULL,
                              variables = NULL,
                              contrast_factor = "reference",
                              contrast_numeric = 1,
                              interaction = FALSE,
                              eps = 1e-4,
                              contrast_types = NULL,
                              ...) {

    lo <- hi <- ter <- lab <- original <- rowid <- list()

    variable_classes <- sapply(variables, function(x) find_variable_class(
                               variable = x,
                               newdata = newdata,
                               model = model))
    if (any(c("factor", "character") %in% variable_classes)) {
        first_interaction <- names(variable_classes[variable_classes %in% c("factor", "character")])[1]
    }
    for (v in variables) {
        # logical and character before factor because they get picked up by find_categorical
        variable_class <- find_variable_class(variable = v,
                                              newdata = newdata,
                                              model = model)

        if (variable_class == "logical") {
            tmp <- get_contrast_data_logical(
                model,
                newdata,
                v,
                ...)

        } else if (variable_class == "factor") {
            if (isTRUE(v %in% names(contrast_types))) {
                ctype <- contrast_types[[v]]
            } else {
                ctype <- contrast_factor
            }
            tmp <- get_contrast_data_factor(
                model,
                newdata,
                v,
                contrast_factor = ctype,
                first_interaction = isTRUE(v == first_interaction),
                ...)

        } else if (variable_class == "character") {
            if (isTRUE(v %in% names(contrast_types))) {
                ctype <- contrast_types[[v]]
            } else {
                ctype <- contrast_factor
            }
            tmp <- get_contrast_data_character(
                model,
                newdata,
                v,
                ctype,
                first_interaction = isTRUE(v == first_interaction),
                ...)

        } else if (variable_class == "numeric") {
            if (isTRUE(v %in% names(contrast_types))) {
                ctype <- contrast_types[[v]]
            } else {
                ctype <- contrast_numeric
            }
            tmp <- get_contrast_data_numeric(
                model,
                newdata,
                v,
                contrast_numeric = ctype,
                eps = eps,
                ...)

        } else {
            stop("variable class not supported.")
        }

        lo[[v]] <- tmp$lo
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
        lo <- rbindlist(lo)
        hi <- rbindlist(hi)
        original <- rbindlist(original)
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
                idx <- setdiff(variables, names(lo)[i])
            } else {
                # exclude rowid and variables excluded from `variables`, for
                # which we do not compute cross-contrasts
                idx <- c(setdiff(names(lo[[i]]), variables),
                         setdiff(variables, names(lo)[[i]]))
            }
            lo[[i]] <- data.table(lo[[i]])[, !..idx]
            hi[[i]] <- data.table(hi[[i]])[, !..idx]
            lo[[i]][[paste0("contrast_", names(lo)[i])]] <- lab[[i]]
            hi[[i]][[paste0("contrast_", names(lo)[i])]] <- lab[[i]]
        }

        lo <- cjdt(lo)
        hi <- cjdt(hi)
        original <- NULL
    }

    out <- list(lo = lo, hi = hi, original = original)

    return(out)
}
