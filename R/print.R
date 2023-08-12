#' Print `marginaleffects` objects
#' 
#' @description
#' This function controls the text which is printed to the console when one of the core `marginalefffects` functions is called and the object is returned: `predictions()`, `comparisons()`, `slopes()`, `marginal_means()`, `hypotheses()`, `avg_predictions()`, `avg_comparisons()`, `avg_slopes()`.
#' 
#' All of those functions return standard data frames. Columns can be extracted by name, `predictions(model)$estimate`, and all the usual data manipulation functions work out-of-the-box:  `colnames()`, `head()`, `subset()`, `dplyr::filter()`, `dplyr::arrange()`, etc.
#' 
#' Some of the data columns are not printed by default. You can disable pretty printing and print the full results as a standard data frame using the `style` argument or by applying `as.data.frame()` on the object. See examples below.
#' 
#' @param x An object produced by one of the [`marginaleffects`] package functions.
#' @param digits The number of digits to display.
#' @param p_eps p values smaller than this number are printed in "<0.001" style.
#' @param topn The number of rows to be printed from the beginning and end of tables with more than `nrows` rows.
#' @param nrows The number of rows which will be printed before truncation.
#' @param ncols The maximum number of column names to display at the bottom of the printed output.
#' @param style "summary" or "data.frame"
#' @param ... Other arguments are currently ignored.
#' @export
#' @examples
#' library(marginaleffects)
#' mod <- lm(mpg ~ hp + am + factor(gear), data = mtcars)
#' p <- predictions(mod, by = c("am", "gear"))
#' p
#' 
#' subset(p, am == 1)
#' 
#' print(p, style = "data.frame")
#' 
#' data.frame(p)
#'
print.marginaleffects <- function(x,
                                  digits = getOption("marginaleffects_print_digits", default = 3),
                                  p_eps = getOption("marginaleffects_print_p_eps", default = 0.001),
                                  topn = getOption("marginaleffects_print_topn", default = 5),
                                  nrows = getOption("marginaleffects_print_nrows", default = 30),
                                  ncols = getOption("marginaleffects_print_ncols", default = 30),
                                  style = getOption("marginaleffects_print_style", default = "summary"),
                                  ...) {


    checkmate::assert_number(digits)
    checkmate::assert_number(topn)
    checkmate::assert_number(nrows)
    checkmate::assert_choice(style, choices = c("data.frame", "summary"))

    if (isTRUE(style == "data.frame")) {
        print(as.data.frame(x))
        return(invisible(x))
    }

    out <- x

    nrows <- max(nrows, 2 * topn)

    if ("group" %in% colnames(out) &&
        all(out$group == "main_marginaleffects")) {
        out$group <- NULL
    }
    
    # subset before rounding so that digits match top and bottom rows
    if (nrow(out) > nrows) {
        out <- rbind(utils::head(out, topn), utils::tail(out, topn))
        splitprint <- TRUE
    } else {
        splitprint <- FALSE
    }

    # round and replace NAs
    ps <- c("p.value", "p.value.nonsup", "p.value.noninf", "p.value.equiv")

    for (i in seq_along(out)) {
        if (colnames(out)[i] %in% ps) {
            out[[i]] <- format.pval(out[[i]], digits = digits, eps = p_eps)
        } else if (isTRUE("s.value" == colnames(out)[i])) {
            out[[i]] <- sprintf("%.1f", out[[i]])
        } else {
            out[[i]] <- format(out[[i]], digits = digits)
        }
    }

    if (is.null(attr(x, "conf_level"))) {
        alpha <- NULL
    } else {
        alpha <- 100 * (1 - attr(x, "conf_level"))
    }

    # contrast is sometimes useless
    if ("contrast" %in% colnames(out) && all(out$contrast == "")) {
        out$contrast <- NULL
    }

    statistic_label <- attr(x, "statistic_label")
    if (is.null(statistic_label)) {
        if (any(out[["df"]] < Inf)) {
            statistic_label <- "t"
        } else {
            statistic_label <- "z"
        }
    }

    # rename
    dict <- c(
        "group" = "Group",
        "term" = "Term",
        "contrast" = "Contrast",
        "value" = "Value",
        "by" = "By",
        "estimate" = "Estimate",
        "std.error" = "Std. Error",
        "statistic" = statistic_label,
        "p.value" = sprintf("Pr(>|%s|)", statistic_label),
        "s.value" = "S",
        "conf.low" = ifelse(is.null(alpha),
            "CI low",
            sprintf("%.1f %%", alpha / 2)),
        "conf.high" = ifelse(is.null(alpha),
            "CI high",
            sprintf("%.1f %%", 100 - alpha / 2)),
        "p.value.nonsup" = "p (NonSup)",
        "p.value.noninf" = "p (NonInf)",
        "p.value.equiv" = "p (Equiv)",
        "df" = "Df",
        "df1" = "Df 1",
        "df2" = "Df 2"
        )


    if (inherits(x, "marginalmeans")) {
        dict["estimate"] <- "Mean"
    }

    # Subset columns
    idx <- c(
        names(dict),
        grep("^contrast_", colnames(x), value = TRUE))

    # explicitly given by user in `datagrid()` or `by` or `newdata`
    nd <- attr(x, "newdata")
    if (is.null(nd)) {
        nd <- attr(x, "newdata_newdata")
    }
    tmp <- c("by",
        attr(nd, "variables_datagrid"),
        attr(nd, "newdata_variables_datagrid"),
        attr(x, "newdata_variables_datagrid")
    )
    if (isTRUE(checkmate::check_character(attr(x, "by")))) {
        tmp <- c(tmp, attr(x, "by"))
    }
    idx <- c(idx[1:grep("by", idx)], tmp, idx[(grep("by", idx) + 1):length(idx)])
    if (isTRUE(attr(nd, "newdata_newdata_explicit")) || isTRUE(attr(nd, "newdata_explicit"))) {
        idx <- c(idx, colnames(nd))
    }

    # drop useless columns: rowid
    useless <- c("rowid", "rowidcf")

    # drop useless columns: dv
    dv <- tryCatch(
        unlist(insight::find_response(attr(x, "model"), combine = TRUE), use.names = FALSE),
        error = function(e) NULL)
    useless <- c(useless, dv)

    # selection style
    data.table::setDT(out)

    # drop useless columns
    idx <- setdiff(unique(idx), useless)
    idx <- intersect(idx, colnames(out))
    out <- out[, ..idx, drop = FALSE]

    if ("term" %in% colnames(out) && all(out$term == "cross")) {
        out[["term"]] <- NULL
        colnames(out) <- gsub("^contrast_", "C: ", colnames(out))
    }

    for (i in seq_along(dict)) {
        colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
    }

    # avoid infinite recursion by stripping marginaleffect.summary class
    data.table::setDF(out)

    # recommend avg_*()
    rec <- ""
    if (isFALSE(attr(x, "by"))) {
        if (inherits(x, "predictions")) {
            rec <- "?avg_predictions and "
        } else if (inherits(x, "comparisons")) {
            rec <- "?avg_comparisons and "
        } else if (inherits(x, "slopes")) {
            rec <- "?avg_slopes and "
        }
    }

    # head
    cat("\n")
    print_head <- attr(x, "print_head")
    if (!is.null(print_head)) {
        cat(print_head, "\n")
    }

    # some commands do not generate average contrasts/mfx. E.g., `lnro` with `by`
    if (splitprint) {
        print(utils::head(out, n = topn), row.names = FALSE)
        msg <- "--- %s rows omitted. See %s?print.marginaleffects ---"
        msg <- sprintf(msg, nrow(x) - 2 * topn, rec)
        cat(msg, "\n")
        # remove colnames
        tmp <- utils::capture.output(print(utils::tail(out, n = topn), row.names = FALSE))
        tmp <- paste(tmp[-1], collapse = "\n")
        cat(tmp)
    } else {
        print(out, row.names = FALSE)
    }
    cat("\n")
    # cat("Model type: ", attr(x, "model_type"), "\n")
    # if (!inherits(x, "hypotheses.summary") && isTRUE(getOption("marginaleffects_print_type", default = TRUE))) {
    #     cat("Prediction type: ", attr(x, "type"), "\n")
    # }
    ## This is tricky to extract nicely when transform_* are passed from avg_comparisons to comparisons. I could certainly figure it out, but at the same time, I don't think the print method should return information that is immediately visible from the call. This is different from `type`, where users often rely on the default value, which can change from model to model, so printing it is often
    # if (!is.null(attr(x, "comparison_label"))) {
    #     cat("Pre-transformation: ", paste(attr(x, "comparison_label"), collapse = ""), "\n")
    # }
    # if (!is.null(attr(x, "transform_label"))) {
    #     cat("Post-transformation: ", paste(attr(x, "transform_label"), collapse = ""), "\n")
    # }
    vg <- attr(x, "variables_grid")
    if (length(vg) > 0) {
        cat(sprintf("Results averaged over levels of: %s",
                    paste(vg, collapse = ", ")), "\n")
    }
    if (ncol(x) <= ncols) {
        cat("Columns:", paste(colnames(x), collapse = ", "), "\n")
    }
    cat("\n")

    print_tail <- attr(x, "print_tail")
    if (!is.null(print_tail)) {
        cat(print_tail, "\n")
    }

    return(invisible(x))
}

#' @noRd
#' @export
print.hypotheses <- print.marginaleffects

#' @noRd
#' @export
print.predictions <- print.marginaleffects

#' @noRd
#' @export
print.comparisons <- print.marginaleffects

#' @noRd
#' @export
print.slopes <- print.marginaleffects

#' @noRd
#' @export
print.marginalmeans <- print.marginaleffects
