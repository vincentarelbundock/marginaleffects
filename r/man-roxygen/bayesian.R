#' @section Bayesian posterior summaries:
#' 
#' By default, credible intervals in bayesian models are built as equal-tailed
#' intervals. This can be changed to a highest density interval by setting a global
#' option:
#' 
#' `options("marginaleffects_posterior_interval" = "eti")`
#' 
#' `options("marginaleffects_posterior_interval" = "hdi")`
#' 
#' By default, the center of the posterior distribution in bayesian models is
#' identified by the median. Users can use a different summary function by setting a
#' global option:
#' 
#' `options("marginaleffects_posterior_center" = "mean")`
#' 
#' `options("marginaleffects_posterior_center" = "median")`
#' 
#' When estimates are averaged using the `by` argument, the `tidy()` function, or
#' the `summary()` function, the posterior distribution is marginalized twice over.
#' First, we take the average *across* units but *within* each iteration of the
#' MCMC chain, according to what the user requested in `by` argument or
#' `tidy()/summary()` functions. Then, we identify the center of the resulting
#' posterior using the function supplied to the
#' `"marginaleffects_posterior_center"` option (the median by default).
#' 
