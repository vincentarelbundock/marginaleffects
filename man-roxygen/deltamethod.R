#' @section Standard errors using the delta method:
#'
#' Standard errors for all quantities estimated by `marginaleffects` can be obtained via the delta method. This requires differentiating a function with respect to the coefficients in the model using a finite difference approach. In some models, the delta method standard errors can be sensitive to various aspects of the numeric differentiation strategy, including the step size. By default, the step size is set to `1e-8`, or to `1e-4` times the smallest absolute model coefficient, whichever is largest.
#'
#' `marginaleffects` can delegate numeric differentiation to the `numDeriv` package, which allows more flexibility. To do this, users can pass arguments to the `numDeriv::jacobian` function through a global option. For example:
#'
#' - `options(marginaleffects_numDeriv = list(method = "simple", method.args = list(eps = 1e-6)))`
#' - `options(marginaleffects_numDeriv = list(method = "Richardson", method.args = list(eps = 1e-5)))`
#' - `options(marginaleffects_numDeriv = NULL)`
#'
#' See the "Uncertainty" chapter on the `marginaleffects` website for more details on the computation of standard errors, bootstrapping, and more:
#'
#' https://marginaleffects.com/chapters/uncertainty.html
#'
