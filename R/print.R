#' Print a marginaleffects object in knitr
#'
#' @keywords internal
#' @return A string with class `knit_asis` to be printed in Rmarkdown or Quarto documents.
#' @rawNamespace S3method(knitr::knit_print, marginaleffects)
knit_print.marginaleffects <- function(x, ...) {
    if (isTRUE(getOption("marginaleffects_print_style") == "tinytable")) {
        insight::check_if_installed("tinytable")
        x <- print(x, "tinytable")
        printfun <- utils::getFromNamespace("knit_print.tinytable", "tinytable")
        printfun(x)
    } else {
        print(x)
    }
}


#' Print `marginaleffects` objects
#'
#' @description
#' This function controls the text which is printed to the console when one of the core `marginalefffects` functions is called and the object is returned: `predictions()`, `comparisons()`, `slopes()`, `hypotheses()`, `avg_predictions()`, `avg_comparisons()`, `avg_slopes()`.
#'
#' All of those functions return standard data frames. Columns can be extracted by name, `predictions(model)$estimate`, and all the usual data manipulation functions work out-of-the-box:  `colnames()`, `head()`, `subset()`, `dplyr::filter()`, `dplyr::arrange()`, etc.
#'
#' Some of the data columns are not printed by default. You can disable pretty printing and print the full results as a standard data frame using the `style` argument or by applying `as.data.frame()` on the object. See examples below.
#'
#' @param x An object produced by one of the `marginaleffects` package functions.
#' @param style "summary", "data.frame", or "tinytable"
#' @param digits The number of digits to display.
#' @param p_eps p values smaller than this number are printed in "<0.001" style.
#' @param topn The number of rows to be printed from the beginning and end of tables with more than `nrows` rows.
#' @param nrows The number of rows which will be printed before truncation.
#' @param ncols The maximum number of column names to display at the bottom of the printed output.
#' @param type boolean: should the type be printed?
#' @param column_names boolean: should the column names be printed?
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
print.marginaleffects <- function(
    x,
    style = getOption("marginaleffects_print_style", default = "summary"),
    digits = getOption("marginaleffects_print_digits", default = 3),
    p_eps = getOption("marginaleffects_print_p_eps", default = 0.001),
    topn = getOption("marginaleffects_print_topn", default = 5),
    nrows = getOption("marginaleffects_print_nrows", default = 30),
    ncols = getOption("marginaleffects_print_ncols", default = 30),
    type = getOption("marginaleffects_print_type", default = TRUE),
    column_names = getOption(
        "marginaleffects_print_column_names",
        default = FALSE
    ),
    ...
) {
    checkmate::assert_number(digits)
    checkmate::assert_number(topn)
    checkmate::assert_number(nrows)
    checkmate::assert_choice(
        style,
        choices = c(
            "data.frame",
            "summary",
            "tinytable",
            "html",
            "latex",
            "markdown",
            "typst"
        )
    )

    mfx <- attr(x, "marginaleffects")

    if (isTRUE(style == "data.frame")) {
        print(as.data.frame(x))
        return(invisible(x))
    }

    print_columns_text <- print_type_text <- print_term_text <- print_contrast_text <- NULL

    out <- x

    nrows <- max(nrows, 2 * topn)

    if ("group" %in% colnames(out) && all(out$group == "main_marginaleffects")) {
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

    conf_level <- if (is.null(mfx)) 0.95 else mfx@conf_level
    alpha <- 100 * (1 - conf_level)

    statistic_label <- attr(x, "statistic_label")
    if (is.null(statistic_label)) {
        if (any(out[["df"]] < Inf)) {
            statistic_label <- "t"
        } else {
            statistic_label <- "z"
        }
    }

    # don't print df if they're all infinite
    if ("df" %in% colnames(out) && all(out$df == Inf, na.rm = TRUE)) {
        out[["df"]] <- NULL
    }

    # rename
    dict <- c(
        "term" = "Term",
        "group" = "Group",
        "contrast" = "Contrast",
        "hypothesis" = "Hypothesis",
        "value" = "Value",
        "by" = "By",
        "estimate" = "Estimate",
        "std.error" = "Std. Error",
        "statistic" = statistic_label,
        "p.value" = sprintf("Pr(>|%s|)", statistic_label),
        "s.value" = "S",
        "conf.low" = sprintf("%.1f %%", alpha / 2),
        "conf.high" = sprintf("%.1f %%", 100 - alpha / 2),
        "pred.low" = sprintf("Pred. %.1f %%", alpha / 2),
        "pred.high" = sprintf("Pred. %.1f %%", 100 - alpha / 2),
        "pred.set" = sprintf("Pred Set %.1f %%", 100 - alpha / 2),
        "p.value.noninf" = "p (NonInf)",
        "p.value.nonsup" = "p (NonSup)",
        "p.value.equiv" = "p (Equiv)",
        "df" = "Df",
        "df1" = "Df 1",
        "df2" = "Df 2",
        "rvar" = "rvar"
    )

    # explicitly given by user in `datagrid()` or `by` or `newdata`
    bycols <- "by"
    if (!is.null(mfx)) {
        bycols <- c("by", mfx@variable_names_by)
    }
    explicit <- c(
        bycols,
        if (!is.null(mfx) && is.data.frame(mfx@newdata)) attr(mfx@newdata, "explicit") else NULL,
        attr(x, "hypothesis_by"),
        attr(x, "newdata_explicit"),
        attr(x, "hypothesis_function_by")
    )

    # useless columns should not be printed
    useless <- c(
        # indices
        "rowid",
        "rowidcf",
        # user-supplied omissions
        getOption("marginaleffects_print_omit", default = NULL),
        # response variable
        if (!is.null(mfx)) mfx@variable_names_response else NULL
    )

    if ("term" %in% colnames(out) && length(unique(out$term)) == 1L) {
        print_term_text <- sprintf("Term: %s\n", out[["term"]][1L])
        useless <- c(useless, "term")
    }

    if ("contrast" %in% colnames(out) && length(unique(out$contrast)) == 1L) {
        print_contrast_text <- sprintf("Comparison: %s\n", out[["contrast"]][1L])
        useless <- c(useless, "contrast")
    }

    # Subset columns
    implicit <- if (!is.null(mfx) && is.data.frame(mfx@newdata)) attr(mfx@newdata, "implicit") else NULL
    idx <- c(
        explicit,
        names(dict),
        grep("^contrast_", colnames(x), value = TRUE)
    )
    start <- grep(
        "term|^contrast|group",
        c(names(dict), colnames(x)),
        value = TRUE
    )
    middle <- explicit
    end <- setdiff(intersect(names(dict), colnames(x)), c(start, middle))
    end <- c(end, implicit)
    idx <- c(start, middle, end)
    idx <- intersect(idx, colnames(out))
    idx <- setdiff(idx, useless)
    idx <- unique(idx)
    out <- data.table(out)[, ..idx, drop = FALSE]

    # rename columns
    old <- colnames(out)
    new <- gsub("^contrast_", "C: ", old)
    idx_match <- match(old, names(dict))
    new[!is.na(idx_match)] <- dict[idx_match[!is.na(idx_match)]]
    data.table::setnames(out, old = old, new = new)

    # Footnotes
    if (ncol(x) <= ncols && isTRUE(column_names)) {
        print_columns_text <- sprintf("Columns: %s\n", toString(colnames(x)))
    }

    if (isTRUE(type) && !is.null(mfx) && !is.null(mfx@type)) {
        print_type_text <- sprintf("Type: %s\n", mfx@type)
    }

    # avoid infinite recursion by stripping marginaleffect.summary class
    data.table::setDF(out)

    if (style %in% c("tinytable", "html", "latex", "typst", "markdown")) {
        insight::check_if_installed("tinytable")

        tab <- as.data.frame(out)

        if (isTRUE(splitprint)) {
            tab <- rbind(utils::head(tab, topn), utils::tail(tab, topn))
        }

        args <- list(x = tab)
        notes <- c(print_type_text, print_columns_text)
        if (!is.null(notes)) args$notes <- notes
        tab <- do.call(tinytable::tt, args)
        tab <- tinytable::format_tt(tab, escape = TRUE)

        if (isTRUE(splitprint)) {
            msg <- "%s rows omitted"
            msg <- sprintf(msg, nrow(x) - 2 * topn)
            msg <- stats::setNames(list(topn + 1), msg)
            tab <- tinytable::group_tt(tab, i = msg)
            tab <- tinytable::style_tt(tab, i = topn + 1, align = "c")
        }

        tab@output <- style
        if (style == "tinytable") {
            return(tab)
        }
        print(tab)
        return(invisible(tab))
    }

    # head
    cat("\n")
    print_head <- attr(x, "print_head")
    if (!is.null(print_head)) {
        cat(print_head, "\n")
    }

    # some commands do not generate average contrasts/mfx. E.g., `lnro` with `by`
    if (splitprint) {
        tmp <- utils::capture.output(print(out, row.names = FALSE))

        top <- paste(tmp[seq_len(topn + 1)], collapse = "\n")
        cat(top)

        msg <- "\n--- %s rows omitted. See ?print.marginaleffects ---\n"
        msg <- sprintf(msg, nrow(x) - 2 * topn)
        cat(msg)

        bottom <- paste(tmp[-seq_len(topn + 1)], collapse = "\n")
        cat(bottom)
    } else {
        print(out, row.names = FALSE)
    }
    cat("\n")

    cat(print_term_text)
    cat(print_type_text)
    cat(print_contrast_text)
    cat(print_columns_text)
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
#' @exportS3Method knitr::knit_print
knit_print.hypotheses <- knit_print.marginaleffects

#' @noRd
#' @exportS3Method knitr::knit_print
knit_print.predictions <- knit_print.marginaleffects

#' @noRd
#' @exportS3Method knitr::knit_print
knit_print.comparisons <- knit_print.marginaleffects

#' @noRd
#' @exportS3Method knitr::knit_print
knit_print.slopes <- knit_print.marginaleffects

