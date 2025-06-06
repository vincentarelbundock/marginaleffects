% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.marginaleffects}
\alias{print.marginaleffects}
\title{Print \code{marginaleffects} objects}
\usage{
\method{print}{marginaleffects}(
  x,
  style = getOption("marginaleffects_print_style", default = "summary"),
  digits = getOption("marginaleffects_print_digits", default = 3),
  p_eps = getOption("marginaleffects_print_p_eps", default = 0.001),
  topn = getOption("marginaleffects_print_topn", default = 5),
  nrows = getOption("marginaleffects_print_nrows", default = 30),
  ncols = getOption("marginaleffects_print_ncols", default = 30),
  type = getOption("marginaleffects_print_type", default = TRUE),
  column_names = getOption("marginaleffects_print_column_names", default = FALSE),
  ...
)
}
\arguments{
\item{x}{An object produced by one of the \code{marginaleffects} package functions.}

\item{style}{"summary", "data.frame", or "tinytable"}

\item{digits}{The number of digits to display.}

\item{p_eps}{p values smaller than this number are printed in "<0.001" style.}

\item{topn}{The number of rows to be printed from the beginning and end of tables with more than \code{nrows} rows.}

\item{nrows}{The number of rows which will be printed before truncation.}

\item{ncols}{The maximum number of column names to display at the bottom of the printed output.}

\item{type}{boolean: should the type be printed?}

\item{column_names}{boolean: should the column names be printed?}

\item{...}{Other arguments are currently ignored.}
}
\description{
This function controls the text which is printed to the console when one of the core \code{marginalefffects} functions is called and the object is returned: \code{predictions()}, \code{comparisons()}, \code{slopes()}, \code{hypotheses()}, \code{avg_predictions()}, \code{avg_comparisons()}, \code{avg_slopes()}.

All of those functions return standard data frames. Columns can be extracted by name, \code{predictions(model)$estimate}, and all the usual data manipulation functions work out-of-the-box:  \code{colnames()}, \code{head()}, \code{subset()}, \code{dplyr::filter()}, \code{dplyr::arrange()}, etc.

Some of the data columns are not printed by default. You can disable pretty printing and print the full results as a standard data frame using the \code{style} argument or by applying \code{as.data.frame()} on the object. See examples below.
}
\examples{
library(marginaleffects)
mod <- lm(mpg ~ hp + am + factor(gear), data = mtcars)
p <- predictions(mod, by = c("am", "gear"))
p

subset(p, am == 1)

print(p, style = "data.frame")

data.frame(p)

}
