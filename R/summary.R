#' Summarize a `marginaleffects` object
#'
#' @param object An object produced by the `marginaleffects` function
#' @inheritParams marginaleffects
#' @inheritParams tidy.marginaleffects
#' @export
summary.marginaleffects <- function(object, conf.level = 0.95, ...) {
    out <- tidy(object, conf.level = conf.level, ...)
    class(out) <- c("marginaleffects.summary", class(out))
    attr(out, "numDeriv_method") <- attr(object, "numDeriv_method")
    attr(out, "type") <- attr(object, "type")
    attr(out, "model_type") <- attr(object, "model_type")
    return(out)
}


#' @export
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
                                "CI low", 
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
