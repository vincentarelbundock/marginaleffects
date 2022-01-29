#' @section Model-Specific Arguments:
#'
#' Some model types allow model-specific arguments to modify the nature of
#' marginal effects, predictions, marginal means, and contrasts.
#'
#' | Package | Class     | Argument         | Documentation |
#' |---------|-----------|------------------|---------------|
#' | `brms`  | `brmsfit` | `ndraws`         | [brms::posterior_predict]|
#' |         |           | `re_formula`     ||
#' | `lme4`  | `merMod`  | `include_random` |[insight::get_predicted]|
#'
