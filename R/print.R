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

    # title
    tit <- "Average marginal effects"

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

    for (i in seq_along(dict)) {
        colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
    }


    # avoid infinite recursion by stripping marginaleffect.summary class
    out <- as.data.frame(out)

    # some commands do not generate average contrasts/mfx. E.g., `lnro` with `by`
    # cat(tit)
    print(out)
    cat("\n")
    cat("Model type: ", attr(x, "model_type"), "\n")
    cat("Prediction type: ", attr(x, "type"), "\n")
    if (!is.null(attr(x, "transform_pre_label"))) {
        cat("Pre-transformation: ", paste(attr(x, "transform_pre_label"), collapse = ""), "\n")
    }
    if (!is.null(attr(x, "transform_post_label"))) {
        cat("Post-transformation: ", paste(attr(x, "transform_post_label"), collapse = ""), "\n")
    }
    if (!is.null(attr(x, "transform_average_label"))) {
        cat("Average-transformation: ", paste(attr(x, "transform_average_label"), collapse = ""), "\n")
    }


    return(invisible(x))
}


#' @noRd
#' @export
print.predictions.summary <- function(x,
                                      digits = max(3L, getOption("digits") - 3L),
                                      ...) {

  out <- x

  # title
  tit <- "Average Adjusted Predictions"

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
  dict <- c("group" = "Group",
            "term" = "Term",
            "estimate" = "Predicted",
            "std.error" = "Std. Error",
            "statistic" = "z",
            "p.value" = "Pr(>|z|)",
            "conf.low" = ifelse(is.null(alpha),
                                "CI low",
                                sprintf("%.1f %%", alpha / 2)),
            "conf.high" = ifelse(is.null(alpha),
                                "CI high",
                                sprintf("%.1f %%", 100 - alpha / 2)))

  for (i in seq_along(dict)) {
    colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
  }


  # avoid infinite recursion by stripping marginaleffect.summary class
  out <- as.data.frame(out)

  # some commands do not generate average contrasts/mfx. E.g., `lnro` with `by`
  # cat(tit, "\n")
  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "type"), "\n")
  if (!is.null(attr(x, "transform_pre_label"))) {
      cat("Pre-transformation: ", paste(attr(x, "transform_pre_label"), collapse = ""), "\n")
  }
  if (!is.null(attr(x, "transform_post_label"))) {
      cat("Post-transformation: ", paste(attr(x, "transform_post_label"), collapse = ""), "\n")
  }
  if (!is.null(attr(x, "transform_average_label"))) {
      cat("Average-transformation: ", paste(attr(x, "transform_average_label"), collapse = ""), "\n")
  }


  return(invisible(x))
}


#' @noRd
#' @export
print.marginalmeans.summary <- function(x,
                                        digits = max(3L, getOption("digits") - 3L),
                                        ...) {

  out <- x

  # title
  tit <- "Estimated marginal means"

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


  # rename
  dict <- c("group" = "Group",
            "term" = "Term",
            "contrast" = "Contrast",
            "value" = "Value",
            "estimate" = "Mean",
            "std.error" = "Std. Error",
            "statistic" = "z",
            "p.value" = "Pr(>|z|)",
            "conf.low" = ifelse(is.null(alpha),
                                "CI low",
                                sprintf("%.1f %%", alpha / 2)),
            "conf.high" = ifelse(is.null(alpha),
                                "CI high",
                                sprintf("%.1f %%", 100 - alpha / 2)))

  for (i in seq_along(dict)) {
    colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
  }

  # avoid infinite recursion by stripping marginaleffect.summary class
  out <- as.data.frame(out)

  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "type"), "\n")

  vg <- attr(x, "variables_grid")
  if (length(vg) > 0) {
    cat(sprintf("Results averaged over levels of: %s",
                paste(vg, collapse = ", ")), "\n")
  }
  if (!is.null(attr(x, "transform_pre_label"))) {
      cat("Pre-transformation: ", paste(attr(x, "transform_pre_label"), collapse = ""), "\n")
  }
  if (!is.null(attr(x, "transform_post_label"))) {
      cat("Post-transformation: ", paste(attr(x, "transform_post_label"), collapse = ""), "\n")
  }
  if (!is.null(attr(x, "transform_average_label"))) {
      cat("Average-transformation: ", paste(attr(x, "transform_average_label"), collapse = ""), "\n")
  }


  return(invisible(x))
}


#' @noRd
#' @export
print.comparisons.summary <- function(x,
                                      digits = max(3L, getOption("digits") - 3L),
                                      ...) {

  out <- x


  # title
  tit <- "Average contrasts"

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
  dict <- c("group" = "Group",
            "term" = "Term",
            "contrast" = "Contrast",
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

  if (all(out$term == "interaction")) {
    out[["term"]] <- NULL
    colnames(out) <- gsub("^contrast_", "", colnames(out))
  }

  for (i in seq_along(dict)) {
    colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
  }


  # avoid infinite recursion by stripping marginaleffect.summary class
  out <- as.data.frame(out)

  # some commands do not generate average contrasts/mfx. E.g., `lnro` with `by`
  # cat(tit, "\n")
  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "type"), "\n")
  if (!is.null(attr(x, "transform_pre_label"))) {
      cat("Pre-transformation: ", paste(attr(x, "transform_pre_label"), collapse = ""), "\n")
  }
  if (!is.null(attr(x, "transform_post_label"))) {
      cat("Post-transformation: ", paste(attr(x, "transform_post_label"), collapse = ""), "\n")
  }
  if (!is.null(attr(x, "transform_average_label"))) {
      cat("Average-transformation: ", paste(attr(x, "transform_average_label"), collapse = ""), "\n")
  }


  return(invisible(x))
}

