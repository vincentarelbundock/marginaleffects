#' Marginal Means
#'
#' @export
marginalmeans <- function(model, 
                          variables,
                          vcov = insight::get_varcov(model)) {


    checkmate::assert_character(variables, min.len = 1)

    newdata <- predictions(model = model, variables = variables)

    # variable selection
    column_labels <- colnames(newdata)
    term_labels <- insight::find_terms(model, flatten = TRUE)
    variables_valid <- intersect(column_labels, term_labels)
    idx <- sapply(variables_valid, function(x) 
        is.factor(newdata[[x]]) || is.logical(newdata[[x]]) || is.character(newdata[[x]]))
    variables_valid <- variables_valid[idx]
    if (is.null(variables)) {
        variables <- variables_valid
    } else {
        variables <- intersect(variables, variables_valid)
    }
    if (length(variables_valid) == 0) {
        stop("No logical, factor, or character variable appears as a column in `newdata` and as an untransformed term in the model formula. If you are converting variables to `factor` in the formula, you might consider converting them in your dataset before fitting the model.") 
    }

    # model.matrix requires a dataset with response
    newdata[[insight::find_response(model)[1]]] <- 0
    mm <- insight::get_modelmatrix(model, data = newdata)

    # marginal means and standard errors for a single variable
    get_mm_se <- function(v) {
        mm_tmp <- mm

        # assign columns of the matrix unrelated to v to their mean value
        idx <- grep(match(v, attr(terms(model), "term.labels")), attr(mm_tmp, "assign"))
        idx <- setdiff(1:ncol(mm_tmp), idx)
        for (i in idx) {
            mm_tmp[, i] <- mean(mm_tmp[, i])
        }

        # one row per combination of the categorical variable
        mm_tmp<- unique(mm_tmp)

        # marginal means
        f <- as.formula(paste("predicted ~", v))
        yhat <- stats::aggregate(f, data = newdata, FUN = mean)
        colnames(yhat) <- c("value", "predicted")
        yhat$term <- v

        # variance: M V M'
        se <- data.frame(
            term = v,
            value = unique(newdata[[v]]),
            std.error = sqrt(diag(mm_tmp %*% vcov %*% t(mm_tmp))))

        # output
        out <- merge(yhat, se)
        out <- out[, c("term", "value", "predicted", "std.error")]
        return(out)
    }

    out_list <- list()
    for (v in variables) {
        out_list[[v]] <- get_mm_se(v)
    }
    out_list <- out_list[sapply(out_list, function(x) !is.null(x))]

    # try to preserve term-value class, but convert if needed to bind
    if (length(out_list) > 1) {
        value_types <- sapply(out_list, function(x) class(x$value)[1])
        if (length(unique(value_types)) > 1) {
            for (i in seq_along(out_list)) {
                out_list[[i]]$value <- as.character(out_list[[i]]$value)
            }
        }
    }
    out <- do.call("rbind", out_list)
    row.names(out) <- NULL

    return(out)
}
