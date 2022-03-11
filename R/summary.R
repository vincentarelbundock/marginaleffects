#' Summarize a `marginaleffects` object
#'
#' @param object An object produced by the `marginaleffects` function
#' @inheritParams marginaleffects
#' @inheritParams tidy.marginaleffects
#' @return Data frame of summary statistics for an object produced by the
#' `marginaleffects` function
#' @export
summary.marginaleffects <- function(object, conf.level = 0.95, ...) {
    out <- tidy(object, conf.level = conf.level, ...)
    class(out) <- c("marginaleffects.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    return(out)
}


#' Print a `marginaleffects` summary
#'
#' @export
#' @noRd
#' @inheritParams summary.marginaleffects
#' @param x an object produced by the `marginaleffects` function.
#' @param digits the number of significant digits to use when printing.
#' @return Printed summary of a `marginaleffects` object
print.marginaleffects.summary <- function(x,
                                          digits = max(3L, getOption("digits") - 3L),
                                          ...) {

  out <- x

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


  if (is.null(attr(x, "conf.level"))) {
      alpha <- NULL
  } else {
      alpha <- 100 * (1 - attr(x, "conf.level"))
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
            "statistic" = "z value",
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

  cat(tit, "\n")
  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "type"), "\n")

  return(invisible(x))
}



#' Summarize a `marginalmeans` object
#'
#' @param object An object produced by the `marginalmeans` function
#' @inheritParams marginalmeans
#' @inheritParams tidy.marginalmeans
#' @return Data frame of summary statistics for an object produced by the
#' `marginalmeans` function
#' @export
summary.marginalmeans <- function(object, conf.level = 0.95, ...) {
    out <- tidy(object, conf.level = conf.level, ...)
    class(out) <- c("marginalmeans.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    return(out)
}


#' Print a `marginalmeans` summary
#'
#' @export
#' @noRd
#' @inheritParams summary.marginalmeans
#' @param x an object produced by the `marginaleffects` function.
#' @param digits the number of significant digits to use when printing.
#' @return Printed summary of a `marginalmeans` object
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


  if (is.null(attr(x, "conf.level"))) {
      alpha <- NULL
  } else {
      alpha <- 100 * (1 - attr(x, "conf.level"))
  }

  # rename
  dict <- c("group" = "Group",
            "term" = "Term",
            "contrast" = "Contrast",
            "value" = "Value",
            "estimate" = "Mean",
            "std.error" = "Std. Error",
            "statistic" = "z value",
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

  cat(tit, "\n")
  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "type"), "\n")

  return(invisible(x))
}



#' Summarize a `predictions` object
#'
#' @param object An object produced by the `predictions` function
#' @inheritParams predictions
#' @inheritParams tidy.predictions
#' @return Data frame of summary statistics for an object produced by the
#' `predictions` function
#' @export
summary.predictions <- function(object, ...) {
    out <- tidy(object, ...)
    class(out) <- c("predictions.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    return(out)
}


#' Print a `predictions` summary
#'
#' @export
#' @noRd
#' @inheritParams summary.predictions
#' @param x an object produced by the `predictions` function.
#' @param digits the number of significant digits to use when printing.
#' @return Printed summary of a `predictions` object
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


  if (is.null(attr(x, "conf.level"))) {
      alpha <- NULL
  } else {
      alpha <- 100 * (1 - attr(x, "conf.level"))
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
            "statistic" = "z value",
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

  cat(tit, "\n")
  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "type"), "\n")

  return(invisible(x))
}






#' Summarize a `comparisons` object
#'
#' @param object An object produced by the `comparisons` function
#' @inheritParams comparisons
#' @inheritParams tidy.comparisons
#' @return Data frame of summary statistics for an object produced by the
#' `comparisons` function
#' @export
summary.comparisons <- function(object, conf.level = 0.95, ...) {
    out <- tidy(object, conf.level = conf.level, ...)
    class(out) <- c("comparisons.summary", class(out))
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    return(out)
}


#' Print a `comparisons` summary
#'
#' @export
#' @noRd
#' @inheritParams summary.comparisons
#' @param x an object produced by the `comparisons` function.
#' @param digits the number of significant digits to use when printing.
#' @return Printed summary of a `comparisons` object
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


  if (is.null(attr(x, "conf.level"))) {
      alpha <- NULL
  } else {
      alpha <- 100 * (1 - attr(x, "conf.level"))
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
            "statistic" = "z value",
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

  cat(tit, "\n")
  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "type"), "\n")

  return(invisible(x))
}






# BAD IDEA
# this forces me to do: head(data.frame(mfx))
# also breaks when I process a marginaleffects data.frame with dplyr, because
# the output retains the class even when it drops columns, and then tidy
# breaks.
#print.marginaleffects <- function(
#    x, digits = max(3L, getOption("digits") - 2L), ...) {
#    print.marginaleffects.summary(
#        summary.marginaleffects(x),
#        digits = digits,
#        ...)
#}
