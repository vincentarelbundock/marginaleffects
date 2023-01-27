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
                                  digits = max(3L, getOption("digits") - 3L),
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

    # round and replace NAs
    for (col in c("estimate", "std.error", "statistic", "conf.low", "conf.high")) {
        if (col %in% colnames(out)) {
            out[[col]] <- format(out[[col]], digits = digits)
        }
    }

    for (p in c("p.value", "p.value.sup", "p.value.inf", "p.value.equ")) {
        if (p %in% colnames(out)) {
            out[[p]] <- format.pval(out[[p]])
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

    if ("type" %in% colnames(out) && length(unique(out$type)) == 1) {
        out$type <- NULL
    }

    # rename
    dict <- c(
        "group" = "Group",
        "term" = "Term",
        "by" = "By",
        "contrast" = "Contrast",
        "value" = "Value",
        "estimate" = "Estimate",
        "std.error" = "Std. Error",
        "statistic" = "z",
        "p.value" = "Pr(>|z|)",
        "conf.low" = ifelse(is.null(alpha),
            "CI low",
            sprintf("%.1f %%", alpha / 2)),
        "conf.high" = ifelse(is.null(alpha),
            "CI high",
            sprintf("%.1f %%", 100 - alpha / 2)),
        "p.value.nonsup" = "p (Sup)",
        "p.value.noninf" = "p (Inf)",
        "p.value.equiv" = "p (Eq)"
        )

    if (inherits(x, "marginalmeans")) {
        dict["estimate"] <- "Mean"
    }

    # Subset columns
    idx <- c(
        names(dict),
        grep("^contrast_", colnames(x), value = TRUE),
        attr(x, "newdata_variables_datagrid"))
    if (isTRUE(checkmate::check_character(attr(x, "by")))) {
        idx <- c(idx, attr(x, "by"))
    }
    out <- out[, colnames(out) %in% idx, drop = FALSE]

    if (all(out$term == "cross")) {
        out[["term"]] <- NULL
        colnames(out) <- gsub("^contrast_", "C: ", colnames(out))
    }

    for (i in seq_along(dict)) {
        colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
    }

    # avoid infinite recursion by stripping marginaleffect.summary class
    out <- as.data.frame(out)

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

    # some commands do not generate average contrasts/mfx. E.g., `lnro` with `by`
    cat("\n")
    if (nrow(out) > nrows) {
        print(utils::head(out, n = topn), row.names = FALSE)
        msg <- "--- %s rows omitted. See %s?print.marginaleffects ---"
        msg <- sprintf(msg, nrow(out) - 2 * topn, rec)
        cat(msg, "\n")
        # remove colnames
        tmp <- utils::capture.output(print(utils::tail(out, n = topn), row.names = FALSE))
        tmp <- paste(utils::tail(tmp, -1), collapse = "\n")
        cat(tmp, "\n")
        omitted <- TRUE
    } else {
        print(out, row.names = FALSE)
        omitted <- FALSE
    }
    cat("\n")
    # cat("Model type: ", attr(x, "model_type"), "\n")
    if (!inherits(x, "hypotheses.summary")) {
        cat("Prediction type: ", attr(x, "type"), "\n")
    }
    if (!is.null(attr(x, "transform_pre_label"))) {
        cat("Pre-transformation: ", paste(attr(x, "transform_pre_label"), collapse = ""), "\n")
    }
    if (!is.null(attr(x, "transform_post_label"))) {
        cat("Post-transformation: ", paste(attr(x, "transform_post_label"), collapse = ""), "\n")
    }
    vg <- attr(x, "variables_grid")
    if (length(vg) > 0) {
        cat(sprintf("Results averaged over levels of: %s",
                    paste(vg, collapse = ", ")), "\n")
    }
    if (ncol(x) <= ncols) {
        cat("Columns:", paste(colnames(x), collapse = ", "), "\n")
    }
    cat("\n")

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
