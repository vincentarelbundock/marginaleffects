get_modeldata <- function(model) {
    out <- hush(insight::get_data(model, verbose = FALSE, additional_variables = TRUE))
    out <- set_variable_class(modeldata = out, model = model)
    return(out)
}

set_variable_class <- function(modeldata, model = NULL) {

    if (is.null(modeldata)) return(modeldata)

    out <- modeldata

    cl <- NULL
    for (col in colnames(out)) {
        if (is.logical(out[[col]])) {
            cl[col] <- "logical"
        } else if (is.character(out[[col]])) {
            cl[col] <- "character"
        } else if (is.factor(out[[col]])) {
            cl[col] <- "factor"
        } else if (is.numeric(out[[col]])) {
            # if (any(!out[[col]] %in% 0:1)) {
                cl[col] <- "numeric"
        #     } else {
        #         cl[col] <- "binary"
        #     }
        } else {
            cl[col] <- "other"
        }
    }
    
    if (is.null(model)) {
        attr(out, "marginaleffects_variable_class") <- cl
        return(out)
    }

    te <- insight::find_terms(model, flatten = TRUE)

    # in-formula factor
    regex <- "^(^as\\.factor|^factor)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te))
    cl[names(cl) %in% idx] <- "factor"

    # in-formula categoricals
    regex <- "^(^mo|^strata)\\((.*)\\)"
    idx <- gsub(
        regex,
        "\\2",
        Filter(function(x) grepl(regex, x), te))
    cl[names(cl) %in% idx] <- "strata"

    # in-formula logical
    regex <- "^logical\\((.*)\\)$|^as.logical\\((.*)\\)$"
    idx <- gsub(
        regex,
        "\\1",
        Filter(function(x) grepl(regex, x), te))
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

    # attributes
    attr(out, "marginaleffects_variable_class") <- cl

    return(out)
}


get_variable_class <- function(newdata, variable = NULL, compare = NULL) {

    if ("marginaleffects_variable_class" %in% names(attributes(newdata))) {
        cl <- attributes(newdata)$marginaleffects_variable_class
    } else {
        newdata <- set_variable_class(newdata)
        cl <- attributes(newdata)$marginaleffects_variable_class
    }

    if (is.null(compare) && is.null(variable)) {
        out <- cl

    } else if (is.null(compare)) {
        out <- cl[variable]

    } else if (is.null(variable)) {
        if (isTRUE(compare == "categorical")) {
            out <- cl[cl %in% c("factor", "character", "logical", "strata", "cluster", "binary")]
        } else {
            out <- cl[cl %in% compare]
        }

    } else {
        if (isTRUE(compare == "categorical")) {
            out <- cl[variable] %in% c("factor", "character", "logical", "strata", "cluster", "binary")
        } else {
            out <- cl[variable] %in% compare
        }
    }

    return(out)
}