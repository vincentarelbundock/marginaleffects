#' @export
summary.marginaleffects <- function(x, 
                                    agg_fun = mean,
                                    ...) {
    out <- tidy(x, agg_fun = agg_fun)
    class(out) <- c("marginaleffects.summary", class(out))
    attr(out, "agg_fun") <- agg_fun
    attr(out, "numDeriv_method") <- attr(x, "numDeriv_method")
    attr(out, "prediction_type") <- attr(x, "prediction_type")
    attr(out, "model_type") <- attr(x, "model_type")
    return(out)
}


#' @export
print.marginaleffects.summary <- function(x, 
                                          digits = max(3L, getOption("digits") - 2L),
                                          ...) {

  out <- x

  # title
  if (identical(mean, attr(out, "agg_fun"))) {
    tit <- "Average marginal effects"
  } else if (identical(median, attr(out, "agg_fun"))) {
    tit <- "Median marginal effects"
  } else {
    tit <- "Marginal effects"
  }

  # round
  for (col in c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")) {
    if (col %in% colnames(out)) {
      out[[col]] <- ifelse(is.na(out[[col]]), 
                           "",
                           format(round(out[[col]], digits = digits)))
    }
  }

  # rename
  dict <- c("term" = " ",
            "estimate" = "Marg. Effect",
            "std.error" = "Std. Error",
            "statistic" = "z value",
            "p.value" = "Pr(>|z|)",
            "conf.low" = "2.5 %",
            "conf.high" = "97.5 %")

  for (i in seq_along(dict)) {
    colnames(out)[colnames(out) == names(dict)[i]] <- dict[i]
  }

  # avoid infinite recursion by stripping marginaleffect.summary class
  out <- as.data.frame(out)

  cat(tit, "\n")
  print(out)
  cat("\n")
  cat("Model type: ", attr(x, "model_type"), "\n")
  cat("Prediction type: ", attr(x, "prediction_type"), "\n")

  return(invisible(x))
}
