get_modeldata <- function(model, ...) {
    out <- insight::get_data(model, verbose = FALSE, additional_variables = TRUE)
    out <- unpack_matrix_1col(out)
    if (is.null(out)) {
        return(NULL)
    }
    data.table::setDF(out)
    return(out)
}


detect_variable_class <- function(modeldata, model = NULL) {
    if (is.null(modeldata)) {
        return(character())
    }

    # this can be costly on large datasets, when only a portion of
    # variables are used in the model
    variables <- NULL
    if (is.null(model)) {
        variables <- tryCatch(
            unlist(insight::find_variables(model, flatten = TRUE, verbose = FALSE), use.names = FALSE),
            error = function(e) NULL
        )
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
        } else if (is.numeric(out[[col]])) {
            if (is_binary(out[[col]])) {
                cl[col] <- "binary"
            } else {
                cl[col] <- "numeric"
            }
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
        out <- if (variable %in% names(cl)) cl[variable] else NA
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
        if (variable %in% names(cl)) {
            if (isTRUE(compare == "categorical")) {
                out <- cl[variable] %in%
                    c("factor", "character", "logical", "strata", "cluster", "binary")
            } else {
                out <- cl[variable] %in% compare
            }
        } else {
            out <- FALSE
        }
    }

    return(out)
}
