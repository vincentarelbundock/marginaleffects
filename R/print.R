# BAD IDEA: do not export print.slopes() et al., which forces head(data.frame(slopes(model)))
# also breaks when I process a marginaleffects data.frame with dplyr, because
# the output retains the class even when it drops columns, and then tidy
# breaks.


#' @noRd
#' @export
print.slopes.summary <- function(x,
                                 digits = max(3L, getOption("digits") - 3L),
                                 ...) {
    out <- x

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
    if ("p.value" %in% colnames(out)) {
        out[["p.value"]] <- format.pval(out[["p.value"]])
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
        "contrast" = "Contrast",
        "value" = "Value",
        "estimate" = "Effect",
        "std.error" = "Std. Error",
        "statistic" = "z",
        "p.value" = "Pr(>|z|)",
        "conf.low" = ifelse(is.null(alpha),
            "CI low",
            sprintf("%.1f %%", alpha / 2)),
        "conf.high" = ifelse(is.null(alpha),
            "CI high",
            sprintf("%.1f %%", 100 - alpha / 2)))

    if (inherits(x, "marginalmeans.summary")) {
        dict["estimate"] <- "Mean"
    }

    if (all(out$term == "cross")) {
        out[["term"]] <- NULL
        colnames(out) <- gsub("^contrast_", "", colnames(out))
    }

    for (i in seq_along(dict)) {
        colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
    }

    # avoid infinite recursion by stripping marginaleffect.summary class
    out <- as.data.frame(out)

    # some commands do not generate average contrasts/mfx. E.g., `lnro` with `by`
    print(out)
    cat("\n")
    cat("Model type: ", attr(x, "model_type"), "\n")
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

    return(invisible(x))
}


#' @noRd
#' @export
print.predictions.summary <- print.slopes.summary

#' @noRd
#' @export
print.marginalmeans.summary <- print.slopes.summary

#' @noRd
#' @export
print.comparisons.summary <- print.slopes.summary

#' @noRd
#' @export
print.hypotheses.summary <- print.slopes.summary