#' @section Equivalence, Inferiority, Superiority:
#'
#' \eqn{\theta} is an estimate, \eqn{\sigma_\theta} its estimated standard error, and \eqn{[a, b]} are the bounds of the equivalence region supplied to the `equivalence` argument.
#' 
#' The test statistics are:
#' 
#' \itemize{
#' \item Non-inferiority: \eqn{t=(\theta - a)/\sigma_\theta}{t=(\theta - a)/\sigma_\theta}
#' \item Non-superiority: \eqn{t=(\theta - b)/\sigma_\theta}{t=(\theta - b)/\sigma_\theta}
#' }
#' 
#' The p values are:
#' 
#' \itemize{
#' \item Non-inferiority: Upper tail probability
#' \item Non-superiority: Lower tail probability
#' \item Equivalence (TOST): Maximum of the non-inferiority and non-superiority p values.
#' }
#' 
#' Thanks to Russell V. Lenth for the excellent `emmeans` package and documentation which inspired this feature.
#' 
 
