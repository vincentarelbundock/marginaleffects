#' @section Model-Specific Arguments:
#'
#' Some model types allow model-specific arguments to modify the nature of
#' marginal effects, predictions, marginal means, and contrasts. Please report
#' other package-specific `predict()` arguments on Github so we can add them to
#' the table below.
#' 
#' https://github.com/vincentarelbundock/marginaleffects/issues
#'
#' | Package     | Class      | Argument           | Documentation |
#' |-------------|------------|--------------------|---------------|
#' | `brms`      | `brmsfit`  | `ndraws`           |[brms::posterior_predict]|
#' |             |            | `re_formula`       |[brms::posterior_predict]|
#' | `lme4`      | `merMod`   | `re.form`          |[lme4::predict.merMod]|
#' |             |            | `allow.new.levels` |[lme4::predict.merMod]|
#' | `glmmTMB`   | `glmmTMB`  | `re.form`          |[glmmTMB::predict.glmmTMB]|
#' |             |            | `allow.new.levels` |[glmmTMB::predict.glmmTMB]|
#' |             |            | `zitype`           |[glmmTMB::predict.glmmTMB]|
#' | `mgcv`      | `bam`      | `exclude`          |[mgcv::predict.bam]|
#' | `robustlmm` | `rlmerMod` | `re.form`          |[robustlmm::predict.rlmerMod]|
#' |             |            | `allow.new.levels` |[robustlmm::predict.rlmerMod]|
#' | `MCMCglmm`  | `MCMCglmm` | `ndraws`           ||
#'
