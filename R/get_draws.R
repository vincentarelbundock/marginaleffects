#' Extract Posterior Draws or Bootstrap Resamples from `marginaleffects` Objects
#'
#' @param x An object produced by a `marginaleffects` package function, such as `predictions()`, `avg_slopes()`, `hypotheses()`, etc.
#' @param shape string indicating the shape of the output format:
#' * "long": long format data frame
#' * "DxP": Matrix with draws as rows and parameters as columns
#' * "PxD": Matrix with draws as rows and parameters as columns
#' * "rvar": Random variable datatype (see `posterior` package documentation).
#' @return A data.frame with `drawid` and `draw` columns.
#' @details
#' If DxP and PxD and the names returned by `coef(x)` are unique, `marginaleffects` sets parameter names to those names. Otherwise, it sets them to `b1`, `b2`, etc.
#' @export
get_draws <- function(x, shape = "long") {
  checkmate::assert_choice(shape, choices = c("long", "DxP", "PxD", "rvar"))

  # tidy.comparisons() sometimes already saves draws in a nice long format
  draws <- attr(x, "posterior_draws")
  if (inherits(draws, "posterior_draws")) {
    return(draws)
  }

  if (is.null(attr(x, "posterior_draws"))) {
    warning('This object does not include a "posterior_draws" attribute. The `posterior_draws` function only supports bayesian models produced by the `marginaleffects` or `predictions` functions of the `marginaleffects` package.',
      call. = FALSE)
    return(x)
  }

  if (nrow(draws) != nrow(x)) {
    stop("The number of parameters in the object does not match the number of parameters for which posterior draws are available.", call. = FALSE)
  }

  if (shape %in% c("PxD", "DxP")) {
    parnames <- names(stats::coef(x))
    if (length(parnames) != nrow(draws) || anyDuplicated(parnames) > 0) {
      parnames <- paste0("b", seq_len(nrow(draws)))
    }
    row.names(draws) <- parnames
    colnames(draws) <- paste0("draw", seq_len(ncol(draws)))
  }

  if (shape == "PxD") {
    return(draws)
  }

  if (shape == "DxP") {
    return(t(draws))
  }

  if (shape == "rvar") {
    insight::check_if_installed("posterior")
    draws <- t(draws)
    if (!is.null(attr(x, "nchains"))) {
      x[["rvar"]] <- posterior::rvar(draws, nchains = attr(x, "nchains"))
    } else {
      x[["rvar"]] <- posterior::rvar(draws)
    }
    return(x)
  }

  if (shape == "long") {
    draws <- data.table(draws)
    setnames(draws, as.character(seq_len(ncol(draws))))
    for (v in colnames(x)) {
      draws[[v]] <- x[[v]]
    }
    out <- melt(
      draws,
      id.vars = colnames(x),
      variable.name = "drawid",
      value.name = "draw")
    cols <- unique(c("drawid", "draw", "rowid", colnames(out)))
    cols <- intersect(cols, colnames(out))
    setcolorder(out, cols)
    data.table::setDF(out)
    return(out)
  }
}


average_draws <- function(data, index, draws, byfun = NULL) {
  insight::check_if_installed("collapse", minimum_version = "2.0.18")

  w <- data[["marginaleffects_wts_internal"]]
  if (all(is.na(w))) {
    w <- NULL
  }

  if (is.null(index)) {
    index <- intersect(colnames(data), "type")
  }

  if (length(index) > 0) {
    if ("term" %in% colnames(data)) {
      Encoding(data[["term"]]) <- "UTF-8"
    }
    g <- collapse::GRP(data, by = index)

    if (is.null(byfun)) {
      draws <- collapse::fmean(
        draws,
        g = g,
        w = w,
        drop = FALSE)
    } else {
      draws <- collapse::BY(
        draws,
        g = g,
        FUN = byfun,
        drop = FALSE)
    }
    out <- data.table(
      g[["groups"]],
      average = collapse::dapply(draws, MARGIN = 1, FUN = collapse::fmedian))
  } else {
    if (is.null(byfun)) {
      draws <- collapse::fmean(
        draws,
        w = w,
        drop = FALSE)
    } else {
      draws <- collapse::BY(
        draws,
        g = g,
        FUN = byfun,
        drop = FALSE)
    }
    out <- data.table(average = collapse::dapply(draws, MARGIN = 1, FUN = collapse::fmedian))
  }

  setnames(out, old = "average", new = "estimate")
  attr(out, "posterior_draws") <- draws
  return(out)
}




#' alias to `get_draws()` keep forever for backward compatibility with JSS
#'
#' @inherit posterior_draws
#' @keywords internal
#' @export
posterior_draws <- get_draws
