library(marginaleffects)
library(nnet)
library(mclogit)
library(emmeans)
library(dplyr)
library(tidyverse)
library(effects)
library(compositions)

softmax <- function(x) exp(x) / sum(exp(x)) 

# TESTS WITH BINOMIAL OUTCOME (WITH MODEL WITH FACTOR+NUMERIC COVARIATE) ####

data(mtcars)
dat = mtcars
dat$cyl = factor(dat$cyl)
dat$am = factor(dat$am)

fit1_glm = glm(am ~ mpg + cyl, family=binomial(logit), data = dat)
fit1_multinom = multinom(am ~ mpg + cyl, data = dat)
fit1_mblogit = mblogit(am ~ mpg + cyl, data = dat)

newdata = data.frame(mpg = c(21), cyl = factor(c(4), levels=levels(dat$cyl))) # with single row
# newdata = data.frame(mpg = c(21, 22), cyl = factor(c(4, 6), levels=levels(dat$cyl))) 
# with multiple rows everything also works as expected

# predict of nnet::multinom with type="probs" drops reference levels
nnet:::predict.multinom(fit1_multinom, newdata=newdata, type="probs")
# 1 
# 0.36026 

# predict of mblogit with type="response" returns all outcome levels
mclogit:::predict.mblogit(fit1_mblogit, newdata=newdata, type="response")
#           0         1
# 1 0.6396388 0.3603612

# EMMEANS results
# emmeans result binomial GLM fit1_glm (CIs backtransformed from link scale)
data.frame(as.data.frame(
  emmeans(fit1_glm, ~ mpg+cyl, at = newdata, type = "link"), 
  type="response"))
# equivalent to emmeans(fit1_glm, ~ mpg+cyl, at = newdata, type = "response")
#   mpg cyl      prob        SE  df asymp.LCL asymp.UCL
# 1  21   4 0.3603612 0.2321165 Inf 0.0725948 0.8021687

# emmeans result for multinomial models fit1_mblogit & fit1_multinom
# these confidence intervals are naive symmetric Delta method intervals
data.frame(emmeans(fit1_mblogit, ~ am|mpg+cyl, at = newdata, mode = "prob"))
#   am mpg cyl      prob        SE  df   asymp.LCL asymp.UCL
# 1  0  21   4 0.6396388 0.2321174 Inf  0.18469699  1.094581
# 2  1  21   4 0.3603612 0.2321174 Inf -0.09458051  0.815303

data.frame(emmeans(fit1_multinom, ~ am|mpg+cyl, at = newdata, mode = "prob", df=Inf)) 
# am mpg cyl    prob        SE  df   asymp.LCL asymp.UCL
# 1  0  21   4 0.63974 0.2320985 Inf  0.18483532 1.0946446
# 2  1  21   4 0.36026 0.2320985 Inf -0.09464463 0.8151647


# MARGINALEFFECTS results

# naive symmeric Delta method CIs on response scale, matches emmeans results
data.frame(predictions(fit1_multinom, 
                       newdata = newdata,
                       type = "probs")) %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error)
# rowid  type group predicted std.error mpg cyl am    conf.low conf.high
# 1     1 probs     1   0.36026 0.2319219  21   4  1 -0.09430687 0.8148269
# note: predict.multinom drops reference level with binary response
# it doesn't do that with >2 outcome levels
# see
# nnet:::predict.multinom(fit1_multinom, newdata=newdata, type="probs") # mistake in predict.multinom
# 1 
# 0.36026

data.frame(predictions(fit1_mblogit, 
                       newdata = newdata,
                       type = "response"))
# CIs are included here because mblogit inherits lm & insight treats it as a lm
# rowid     type group predicted std.error statistic     p.value    conf.low conf.high mpg cyl am
# 1     1 response     0 0.6396388  0.231941  2.757765 0.005819794  0.18504283 1.0942347  21   4  1
# 2     1 response     1 0.3603612  0.231941  1.553677 0.120261562 -0.09423467 0.8149572  21   4  1


# CIs that match emmeans result of binomial GLM, here obtained via
# backtransformation from logit scale for multinomial models fit1_mblogit & fit1_multinom
data.frame(predictions(fit1_multinom, 
                       newdata = newdata,
                       type = "logit") %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error) %>%
             group_by(rowid) |>
             mutate_at(c("predicted", "conf.low", "conf.high"), function (x) plogis(x)))
#   rowid  type group predicted std.error mpg cyl am   conf.low conf.high
# 1     1 logit     0   0.63974  1.007054  21   4  1 0.19788119 0.9274432
# 2     1 logit     1   0.36026  1.007054  21   4  1 0.07255683 0.8021188

# this matches result of Effects package, that also calculates CIs like this
# (well, using Delta method on logit scale,
# see Fox & Anderson 2006, https://siteget.net/o.php?b=4&pv=4&mobile=&u=http%3A%2F%2Flibgen.is%2Fscimag%2F%3Fq%3D10.1111%252Fj.1467-9531.2006.00180.x)
# "EFFECT DISPLAYS FOR MULTINOMIAL AND PROPORTIONAL-ODDS LOGIT MODELS"
preds_effects = data.frame(Effect(focal.predictors=c("mpg", "cyl"), fit1_glm,
                                  xlevels=list(mpg=newdata$mpg)))
preds_effects[preds_effects$mpg==newdata$mpg & as.character(preds_effects$cyl)==as.character(newdata$cyl), ]
#    mpg cyl       fit        se     lower     upper
# 10  21   4 0.3603612 0.2321165 0.0725948 0.8021687

# CIs that match emmeans result of binomial GLM, here obtained via
# backtransformation from link (additive logratio) scale for multinomial models fit1_mblogit & fit1_multi
# (with type="link" reference level is normalized to zero & here made to come out,
# this is also known as the additive logratio scale)
data.frame(predictions(fit1_mblogit, 
            newdata = newdata,
            type = "link") %>%
  # transform(conf.low = predicted - 1.96 * std.error,
  #           conf.high = predicted + 1.96 * std.error) |>
  group_by(rowid) |>
  mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(x)))
#   rowid type group predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1 link     0 0.6396388  0.000000        NaN       NaN 0.92740571 0.1978301  21   4  1
# 2     1 link     1 0.3603612  1.007013 -0.5698008 0.5688128 0.07259429 0.8021699  21   4  1


data.frame(predictions(fit1_multinom, 
                       newdata = newdata,
                       type = "link") %>%
             # transform(conf.low = predicted - 1.96 * std.error,
             #           conf.high = predicted + 1.96 * std.error) |>
             group_by(rowid) |>
             mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(x)))
#   rowid type group predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1 link     0   0.63974  0.000000        NaN       NaN 0.92744073 0.1978869  21   4  1
# 2     1 link     1   0.36026  1.007054 -0.5702135 0.5685329 0.07255927 0.8021131  21   4  1



# predictions on latent (centered logratio) scale
# EMMEANS
data.frame(emmeans(fit1_mblogit, ~ am|mpg+cyl, at = newdata, mode = "latent"))
#   am mpg cyl     emmean        SE  df  asymp.LCL asymp.UCL
# 1  0  21   4  0.2868983 0.5035063 Inf -0.6999558 1.2737524
# 2  1  21   4 -0.2868983 0.5035063 Inf -1.2737524 0.6999558

data.frame(emmeans(fit1_multinom, ~ am|mpg+cyl, at = newdata, mode = "latent"))
#   am mpg cyl     emmean       SE df  lower.CL upper.CL
# 1  0  21   4  0.2871179 0.503527  4 -1.110897 1.685133
# 2  1  21   4 -0.2871179 0.503527  4 -1.685133 1.110897

# MARGINALEFFECTS: matches emmeans
predictions(fit1_mblogit, 
            newdata = newdata,
            type = "clr")
#   rowid type group  predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1  clr     0  0.2868983 0.5035063  0.5698008 0.5688128 -0.6999558 1.2737524  21   4  1
# 2     1  clr     1 -0.2868983 0.5035063 -0.5698008 0.5688128 -1.2737524 0.6999558  21   4  1

predictions(fit1_multinom, 
            newdata = newdata,
            type = "clr") %>%
  transform(conf.low = predicted - 1.96 * std.error,
            conf.high = predicted + 1.96 * std.error)
#   rowid type group  predicted std.error mpg cyl am  conf.low conf.high
# 1     1  clr     0  0.2871179  0.503527  21   4  1 -0.699795  1.274031
# 2     1  clr     1 -0.2871179  0.503527  21   4  1 -1.274031  0.699795


# note: CIs backtransformed from latent (centered logratio) scale to response scale that match
# those of the emmeans binomial GLM can be obtained here using
data.frame(predictions(fit1_mblogit, 
                       newdata = newdata,
                       type = "clr") %>%
             # transform(conf.low = predicted - 1.96 * std.error,
             #           conf.high = predicted + 1.96 * std.error) |>
             group_by(rowid) |>
             mutate_at(c("predicted", "conf.low", "conf.high"), 
                       function (x) softmax(c(0,x[-1])*2)))
#   rowid type group predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1  clr     0 0.6396388 0.5035063  0.5698008 0.5688128 0.92740571 0.1978301  21   4  1
# 2     1  clr     1 0.3603612 0.5035063 -0.5698008 0.5688128 0.07259429 0.8021699  21   4  1


# TESTS WITH MULTINOMIAL OUTCOMES & IN-FORMULA TRANSFORMATION (spline) ####

newdata = data.frame(mpg = c(21), am = factor(c(1), levels=levels(dat$am))) # with single row
# newdata = data.frame(mpg = c(21, 22), am = factor(c(0, 1), levels=levels(dat$am))) 
# with multiple rows everything also works as expected

library(splines)
fit2_multinom = multinom(cyl ~ ns(mpg, df=2) + am, 
                         data = dat[rep(seq_len(nrow(dat)), 100), ], Hess=TRUE) 
# we work with a bigger dataset here (dataset replicated & rbinded 100x
# to be able to check that asymptotic CIs below are correct or not)

# correct CIs determined here using simulation
# as in https://stackoverflow.com/questions/35970480/generating-confidence-intervals-for-predicted-probabilities-after-running-mlogit
library(MASS)

model = fit2_multinom
est_betas <- coef(model)
est_preds <- predict(model, newdata = newdata, type="probs")

set.seed(1)
sim_betas <- mvrnorm(10000, as.vector(t(est_betas)), 
                     Sigma=vcov(model)) # vcov=solve(fit2_multinom$Hessian)
mod = model
simulate <- function (sim_betas, type="response") { 
  apply(sim_betas, 1, function(x) {
  mt <- terms(mod)
  rhs <- delete.response(mt)
  m <- model.frame(rhs, data = newdata, na.action = na.exclude) # na.action = na.omit, xlev = model$xlevels
  X <- model.matrix(rhs, m,
                    contrasts.arg = model$contrasts,
                    xlev = model$xlevels)
  betahat = matrix(x, nrow=nrow(est_betas), byrow=T)
  betahat <- t(rbind(0, betahat)) # model coefficients, with expicit zero row 
  # added for reference category & transposed
  if (type=="response"|type=="logit") pred <- t(apply(X %*% betahat, 1, softmax)) 
  if (type=="logit") pred <- qlogis(pred)
  if (type=="link"|type=="clr") pred <- X %*% betahat # pred on link (additive logratio) scale
  if (type=="clr") pred <- pred - mean(pred) # pred on latent (centered logratio) scale
  colnames(pred) <- model$lev
  pred 
  # m$wts <- x
  # predict(m, newdata = newdata, type="probs")
})
}

sim_preds = simulate(sim_betas, type="response")
sim_ci <- apply(sim_preds, 1, quantile, c(.025, .975))
cbind(prob = est_preds, t(sim_ci))
#          prob         2.5%      97.5%
# 4 0.002922284 0.0002956117 0.02894496
# 6 0.970302558 0.9404743876 0.98178835
# 8 0.026775158 0.0157178578 0.04524799

sim_SE_clr <- apply(simulate(sim_betas, type="clr"), 1, sd)
sim_SE_clr
# 0.7883664 0.4010862 0.4342561

sim_SE_logit <- apply(simulate(sim_betas, type="logit"), 1, sd)
sim_SE_logit
# 1.1732219 0.3134717 0.2783580

sim_SE_response <- apply(simulate(sim_betas, type="response"), 1, sd)
sim_SE_response
# 0.008723380 0.011297339 0.007571004

data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "logit")) # SEs appear approx correct

data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "clr")) # SEs appear approx correct


# better CIs backtransformed from logit scale
# (reference level now dropped but equal to 1-(sum of the rest))
data.frame(predictions(fit2_multinom,  
                       newdata = newdata,
                       type = "logit") %>%
             group_by(rowid) %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error) %>% 
             mutate_at(c("predicted", "conf.low", "conf.high"), 
                       function (x) plogis(x)))
#   rowid  type group  predicted std.error mpg  am cyl     conf.low  conf.high
# 1     1 logit     4 0.002922284 1.1630271  21  1   6 0.0002998296 0.02784316
# 2     1 logit     6 0.970302558 0.2770891  21  1   6 0.9499531142 0.98252993
# 3     1 logit     8 0.026775158 0.2791013  21  1   6 0.0156705227 0.04538606
# these CIs approx match simulated ones above

# this matches result of Effects package, that also calculates CIs like this
# (well, using Delta method on logit scale,
# see Fox & Anderson 2006, https://siteget.net/o.php?b=4&pv=4&mobile=&u=http%3A%2F%2Flibgen.is%2Fscimag%2F%3Fq%3D10.1111%252Fj.1467-9531.2006.00180.x)
# "EFFECT DISPLAYS FOR MULTINOMIAL AND PROPORTIONAL-ODDS LOGIT MODELS")
preds_effects = data.frame(Effect(focal.predictors=c("mpg", "am"), fit2_multinom,
                                  xlevels=list(mpg=newdata$mpg)), check.names = F)
preds_effects[preds_effects$mpg==newdata$mpg & as.character(preds_effects$am)==as.character(newdata$am), ]
# mpg am     prob.X4   prob.X6    prob.X8  logit.X4 logit.X6  logit.X8  se.prob.X4  se.prob.X6
# 31  21  1 0.002922284 0.9703026 0.02677516 -5.832463 3.486547 -3.593141 0.003388782 0.007984292
# se.prob.X8 se.logit.X4 se.logit.X6 se.logit.X8    L.prob.X4 L.prob.X6  L.prob.X8  U.prob.X4
# 31 0.007272892    1.163033   0.2770832   0.2791013 0.0002998385 0.9499541 0.01567068 0.02784236
# U.prob.X6  U.prob.X8 L.logit.X4 L.logit.X6 L.logit.X8 U.logit.X4 U.logit.X6 U.logit.X8
# 31 0.9825296 0.04538562  -8.111967   2.943474  -4.140169   -3.55296    4.02962  -3.046112


# better CIs backtransformed from link (additive logratio) scale
# (reference level now dropped but equal to 1-(sum of the rest))
data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "link") %>%
             # transform(conf.low = predicted - 1.96 * std.error,
             #           conf.high = predicted + 1.96 * std.error) |>
             group_by(rowid) |>
             mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(x)))
# rowid type group   predicted std.error statistic      p.value   conf.low    conf.high mpg am cyl
# 1     1 link     4 0.002922284  0.000000       NaN          NaN 0.02788691 0.0002993164  21  1   6
# 2     1 link     6 0.970302558  1.163056  4.991371 5.995207e-07 0.94752431 0.9712027820  21  1   6
# 3     1 link     8 0.026775158  1.194397  1.854583 6.365579e-02 0.02458879 0.0284979016  21  1   6
# relatively close to correct CIs but CIs backtransformed from logit scale closer
# to simulated ones

data.frame(predictions(fit2_multinom,  
                       newdata = newdata,
                       type = "clr") %>%
             group_by(rowid) %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error) %>% 
             mutate_at(c("predicted", "conf.low", "conf.high"), 
                       function (x) softmax(x)))
# rowid type group   predicted std.error mpg am cyl    conf.low   conf.high
# 1     1  clr     4 0.002922284 0.7803609  21  1   6 0.001388809 0.006138018
# 2     1  clr     6 0.970302558 0.3992502  21  1   6 0.973274608 0.965616957
# 3     1  clr     8 0.026775158 0.4289867  21  1   6 0.025336583 0.028245025
# NOT CORRECT


# emmeans
data.frame(emmeans(fit2_multinom, ~ cyl|mpg+am, at = newdata, mode = "prob", df=Inf)) 
#   cyl mpg am        prob          SE  df    asymp.LCL   asymp.UCL
# 1   4  21  1 0.002922284 0.003388782 Inf -0.003719607 0.009564175
# 2   6  21  1 0.970302558 0.007984292 Inf  0.954653633 0.985951483
# 3   8  21  1 0.026775158 0.007272892 Inf  0.012520551 0.041029765

# marginaleffects
# naive symmeric Delta method CIs on response scale
data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "probs")) %>%
  transform(conf.low = predicted - 1.96 * std.error,
            conf.high = predicted + 1.96 * std.error)
#   rowid  type group   predicted   std.error mpg am cyl    conf.low   conf.high
# 1     1 probs     4 0.002922284 0.003389094  21  1   6 -0.00372034 0.009564909
# 2     1 probs     6 0.970302558 0.007987395  21  1   6  0.95464726 0.985957853
# 3     1 probs     8 0.026775158 0.007273399  21  1   6  0.01251930 0.041031019




# predictions on latent (centered logit scale)
data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "clr") %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error))
# rowid type group  predicted std.error mpg am cyl  conf.low  conf.high
# 1     1  clr     4 -2.6734504 0.7803609  21  1   6 -4.202958 -1.1439430
# 2     1  clr     6  3.1317919 0.3992502  21  1   6  2.349262  3.9143223
# 3     1  clr     8 -0.4583415 0.4289867  21  1   6 -1.299155  0.3824724


# TESTS WITH irl (isometric logratio) SCALE ####
library(compositions)
V = ilrBase(D=ncol(pred_ilr_wide)) # D= nr of response levels
pred_ilr = data.frame(predictions(fit2_multinom, 
                       type = "ilr") %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error), check.names=F)
pred_ilr_wide = data.frame(pred_ilr %>%  # correct
                pivot_wider(id_cols=rowid, names_from = group, 
                            values_from = predicted), check.names=F)
conf_low_ilr_wide = data.frame(pred_ilr %>%  # correct
                pivot_wider(id_cols=rowid, names_from = group, 
                            values_from = conf.low), check.names=F)
conf_high_ilr_wide = data.frame(pred_ilr %>%  # correct
                pivot_wider(id_cols=rowid, names_from = group, 
                             values_from = conf.high), check.names=F)
pred_backtransformed = data.frame(rowid=pred_ilr_wide$rowid, 
                                         ilrInv(pred_ilr_wide[,-1], V=V), 
                                         check.names=F)
colnames(pred_backtransformed)= c("rowid", fit2_multinom$lev)
conf_low_backtransformed = data.frame(rowid=conf_low_ilr_wide$rowid, 
                                  ilrInv(conf_low_ilr_wide[,-1], V=V), 
                                  check.names=F)
colnames(conf_low_backtransformed)= c("rowid", fit2_multinom$lev)
conf_high_backtransformed = data.frame(rowid=conf_high_ilr_wide$rowid, 
                                  ilrInv(conf_high_ilr_wide[,-1], V=V), 
                                  check.names=F)
colnames(conf_high_backtransformed)= c("rowid", fit2_multinom$lev)

pred_backtransformed_long = pred_backtransformed %>%
  pivot_longer(pred_backtransformed, cols=all_of(fit2_multinom$lev), 
               names_to="group", values_to="predicted")
conf_low_backtransformed_long = conf_low_backtransformed %>%
  pivot_longer(conf_low_backtransformed, cols=all_of(fit2_multinom$lev), 
               names_to="group", values_to="conf.low")
conf_high_backtransformed_long = conf_high_backtransformed %>%
  pivot_longer(conf_high_backtransformed, cols=all_of(fit2_multinom$lev), 
               names_to="group", values_to="conf.high")

pred_ilr_backtransformed = left_join(dplyr::select(pred_ilr, -c(predicted)), 
                                     pred_backtransformed_long)
pred_ilr_backtransformed = left_join(dplyr::select(pred_ilr_backtransformed, -c(conf.low)), 
                                     conf_low_backtransformed_long)
pred_ilr_backtransformed = left_join(dplyr::select(pred_ilr_backtransformed, -c(conf.high)), 
                                     conf_high_backtransformed_long)
head(pred_ilr_backtransformed)

pred_ilr_backtransformed[pred_ilr_backtransformed$rowid==1,]


