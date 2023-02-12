#' @section Model-Specific Arguments:
#'
#' Some model types allow model-specific arguments to modify the nature of
#' marginal effects, predictions, marginal means, and contrasts.
#'
#' | Package     | Class      | Argument           | Documentation |
#' |-------------|------------|--------------------|---------------|
#' | `brms`      | `brmsfit`  | `ndraws`           |[brms::posterior_predict]|
#' |             |            | `re_formula`       ||
#' | `lme4`      | `merMod`   | `include_random`   |[insight::get_predicted]|
#' |             |            | `re.form`          |[lme4::predict.merMod]|
#' |             |            | `allow.new.levels` |[lme4::predict.merMod]|
#' | `glmmTMB`   | `glmmTMB`  | `re.form`          |[glmmTMB::predict.glmmTMB]|
#' |             |            | `allow.new.levels` |[glmmTMB::predict.glmmTMB]|
#' |             |            | `zitype`           |[glmmTMB::predict.glmmTMB]|
#' | `mgcv`      | `bam`      | `exclude`          |[mgcv::predict.bam]|
#' | `robustlmm` | `rlmerMod` | `re.form`          |[robustlmm::predict.rlmerMod]|
#' |             |            | `allow.new.levels` |[robustlmm::predict.rlmerMod]|
#' | `MCMCglmm`  | `MCMCglmm` | `ndraws  `         ||
#'
