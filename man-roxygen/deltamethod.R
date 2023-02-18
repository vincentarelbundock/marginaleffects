#' @section Delta method standard errors:
#'
#' Standard errors are obtained using the Delta method and finite differences. In some models, the estimated standard errors can be can be quite sensitive to  the numeric differentiation strategy (e.g., step size). 
#'
#' See the "Standard Errors and Confidence Intervals" vignette on the `marginaleffects` website to learn how standard errors are computed, and how the `numDeriv` package can help you explore the effects of alternative differentiation strategies:
#'
#' https://vincentarelbundock.github.io/marginaleffects/articles/uncertainty.html
