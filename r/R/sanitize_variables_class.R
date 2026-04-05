# Variable class detection and checking

detect_variable_class <- function(modeldata, model = NULL) {
    if (is.null(modeldata)) {
        return(character())
    }

    # this can be costly on large datasets, when only a portion of
    # variables are used in the model
    variables <- NULL
    variables_list <- NULL
    if (!is.null(model)) {
        variables_list <- tryCatch(
            insight::find_variables(model, flatten = FALSE, verbose = FALSE),
            error = function(e) NULL
        )
        variables <- unlist(variables_list, use.names = FALSE)
        # Always include all columns that are actually present in modeldata
        # This ensures that variables used in 'by' arguments get properly classified
        if (!is.null(variables)) {
            variables <- union(variables, colnames(modeldata))
        }
    }
    if (is.null(variables)) variables <- colnames(modeldata)

    out <- modeldata

    cl <- NULL
    for (col in variables) {
        if (is.matrix(out[[col]])) {
            cl[col] <- "matrix"
        } else if (is.logical(out[[col]])) {
            cl[col] <- "logical"
        } else if (is.character(out[[col]])) {
            cl[col] <- "character"
        } else if (is.factor(out[[col]])) {
            cl[col] <- "factor"
        } else if (inherits(out[[col]], "Surv")) {
            # is numeric but breaks the %in% 0:1 check
            cl[col] <- "other"
        } else if (isTRUE(checkmate::check_integerish(out[[col]]))) {
            if (is_binary(out[[col]])) {
                cl[col] <- "binary"
            } else {
                cl[col] <- "integer"
            }
        } else if (is.numeric(out[[col]])) {
            cl[col] <- "numeric"
        } else {
            cl[col] <- "other"
        }
    }

    if (is.null(model)) {
        return(cl)
    }

    te <- hush(insight::find_terms(model, flatten = TRUE))

    # in-formula factor
    regex <- "^(^as\\.factor|^factor)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "factor"

    # in-formula categoricals
    regex <- "^(^mo|^strata)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "strata"

    # in-formula numeric
    regex <- "^numeric\\((.*)\\)$|^as.numeric\\((.*)\\)$"
    idx <- gsub(
        regex,
        "\\1",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "numeric"

    # in-formula logical
    regex <- "^logical\\((.*)\\)$|^as.logical\\((.*)\\)$"
    idx <- gsub(
        regex,
        "\\1",
        Filter(function(x) grepl(regex, x), te)
    )
    cl[names(cl) %in% idx] <- "logical"

    # in-formula: fixest::i()
    fi <- NULL
    idx <- grepl("^i\\(.*\\)$", te)
    if (sum(idx) > 0) {
        arg1 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[2]])))
        arg2 <- lapply(te[idx], function(x) hush(as.character(str2lang(x)[[3]])))
        arg1 <- unlist(arg1, recursive = TRUE)
        arg2 <- unlist(arg2, recursive = TRUE)
        arg2 <- gsub("^i\\.", "", arg2[grepl("^i\\.", arg2)])
        fi <- unique(c(arg1, arg2))
    }
    for (f in fi) {
        cl[f] <- "cluster"
    }

    # random effects groups
    re <- hush(insight::find_random(model, flatten = TRUE))
    cl[names(cl) %in% re] <- "cluster"

    # cluster variables reported by insight (e.g., fixest fixed effects via `|`)
    if (!is.null(variables_list[["cluster"]])) {
        cl[names(cl) %in% variables_list[["cluster"]]] <- "cluster"
    }

    return(cl)
}


check_variable_class <- function(newdata, variable = NULL, compare = NULL) {
    if (inherits(newdata, "marginaleffects_internal")) {
        cl <- newdata@variable_class
    } else if ("marginaleffects_variable_class" %in% names(attributes(newdata))) {
        cl <- attributes(newdata)$marginaleffects_variable_class
    } else if (isTRUE(checkmate::check_character(newdata))) {
        cl <- newdata
    } else {
        cl <- character()
    }

    if (is.null(compare) && is.null(variable)) {
        out <- cl
    } else if (is.null(compare)) {
        # Handle vector of variables
        out <- cl[variable]
        out[!names(out) %in% names(cl)] <- NA
    } else if (is.null(variable)) {
        if (isTRUE(compare == "categorical")) {
            out <- cl[
                cl %in%
                    c("factor", "character", "logical", "strata", "cluster", "binary")
            ]
        } else {
            out <- cl[cl %in% compare]
        }
    } else {
        # Handle vector of variables: use any() to check if ANY variable matches
        vars_in_cl <- variable %in% names(cl)
        if (any(vars_in_cl)) {
            if (isTRUE(compare == "categorical")) {
                out <- any(cl[variable[vars_in_cl]] %in%
                    c("factor", "character", "logical", "strata", "cluster", "binary"))
            } else {
                out <- any(cl[variable[vars_in_cl]] %in% compare)
            }
        } else {
            out <- FALSE
        }
    }

    return(out)
}
