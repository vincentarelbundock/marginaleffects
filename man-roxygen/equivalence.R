#' @section Equivalence, Inferiority, Superiority:
#'
#' \eqn{\theta} is an estimate, \eqn{\sigma_\theta} its estimated standard error, and \eqn{[a, b]} are the bounds of the interval supplied to the `equivalence` argument.
#' 
#' Non-inferiority:
#' 
#' - \eqn{H_0}{H0}: \eqn{\theta \leq a}{\theta <= a}
#' - \eqn{H_1}{H1}: \eqn{\theta > a}
#' - \eqn{t=(\theta - a)/\sigma_\theta}{t=(\theta - a)/\sigma_\theta}
#' - p: Upper-tail probability
#' 
#' Non-superiority:
#' 
#' - \eqn{H_0}{H0}: \eqn{\theta \geq b}{\theta >= b}
#' - \eqn{H_1}{H1}: \eqn{\theta < b}
#' - \eqn{t=(\theta - b)/\sigma_\theta}{t=(\theta - b)/\sigma_\theta}
#' - p: Lower-tail probability
#' 
#' Equivalence: Two One-Sided Tests (TOST)
#' 
#' - p: Maximum of the non-inferiority and non-superiority p values.
#' 
#' Thanks to Russell V. Lenth for the excellent `emmeans` package and documentation which inspired this feature.
#' 
