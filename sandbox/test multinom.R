library(nnet)
library(mclogit)
library(emmeans)
library(dplyr)
library(tidyverse)

# SOME DATA (WITH OUTCOME VARIABLE HERE WITH 2 LEVELS TO FACILITY COMPARISON WITH BINOMIAL GLM) ####

x = 1:1000
n = length(x)
set.seed(1)
y1 = rbinom(n, 10, prob=plogis(-10+0.02*x))
y2 = 10-y1
dat = data.frame(x,y1,y2)
dat_long = dat |>
  pivot_longer(!x, names_to = "y", values_to = "count") |>
  mutate(y = factor(y, levels=c("y2", "y1")))

# BINOMIAL GLM FIT ####

fit_glm = glm(cbind(y1,y2) ~ x, family=binomial(logit), data=dat)
summary(fit_glm)
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   -9.8656352  0.2454748  -40.19   <2e-16 ***
#   x            0.0196292  0.0004804   40.86   <2e-16 ***

# EQUIVALENT nnet::multinom and mclogit::mblogit MULTINOMIAL FITS ####

fit_multinom = nnet::multinom(y ~ x, weights=count, data=dat_long) 
class(fit_multinom) = c(class(fit_multinom), "lm")
fit_mblogit = mclogit::mblogit(y ~ x, weights=count, from.table=FALSE, dispersion=FALSE, data=dat_long)

# below I will work with fit_mblogit, but idea is the same for fit_multinom

summary(fit_mblogit)
# Equation for y1 vs y2:
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -9.8656352  0.2454876  -40.19   <2e-16 ***
#   x            0.0196292  0.0004804   40.86   <2e-16 ***
  
# emmeans binomial GLM fit for row with x=400 ####
# Intervals here are back-transformed from the logit scale 
# this call runs very fast
# NOTE: Ideally I would also like to back-transform from the generalized logit scale for multinomial models
# to avoid having to use the slow delta method on the response/probs scale
emmeans(fit_glm, ~ x, at=list(x=unique(dat$x)), type="response")[400,] 
#   x  prob      SE  df asymp.LCL asymp.UCL
# 400 0.118 0.00688 Inf     0.105     0.132
# 
# Confidence level used: 0.95 
# Intervals are back-transformed from the logit scale 

# previous emmeans call is in fact interpreted as
as.data.frame(emmeans(fit_glm, ~ x, at=list(x=unique(dat$x)), type="link"), 
              type="response")[400,] # Intervals are back-transformed from the logit scale 
#       x      prob          SE  df asymp.LCL asymp.UCL
# 400 400 0.1177451 0.006884648 Inf 0.1049069 0.1319228


# emmeans mclogit::mblogit MULTINOMIAL FIT ####

# emmeans on response scale with SEs calculated on response scale using delta method
# NOTE: this is slow!
system.time(emmeans_multinom_preds_response <- as.data.frame(emmeans(fit_mblogit, ~ y, by="x", at=list(x=unique(dat$x)), 
                                                                     mode="prob", df=Inf), 
                                                             type="response")) 
# 20s, Intervals calculated on response scale using delta method

emmeans_multinom_preds_response[emmeans_multinom_preds_response$x==400,]
#      y   x      prob          SE  df asymp.LCL asymp.UCL
# 799 y2 400 0.8822549 0.006884906 Inf 0.8687608 0.8957491
# 800 y1 400 0.1177451 0.006884906 Inf 0.1042509 0.1312392
# note: 0.1177451 [0.1042509-0.1312392] 95% CLs is very close to result 
# 0.1177451 [0.1049069-0.1319228] 95% CLs from binomial GLM above, as expected

# if I could calculate conf intervals on latent scale & then backtransform this would be 133x faster though
# (cf emmeans runtime: 0.15s with mode="latent" vs 20s for mode="prob")
system.time(as.data.frame(emmeans(fit_mblogit, ~ y, by="x", at=list(x=unique(dat$x)), 
                                  mode="prob", df=Inf), type="response")) # 20s
system.time(as.data.frame(emmeans(fit_mblogit, ~ y, by="x", at=list(x=unique(dat$x)), 
                      mode="latent", df=Inf), type="link")) # 0.15s

# result I had hoped to get without having to use the (slow) delta method:
# this is ca. 400x faster than the emmeans call above
# here I will use the latest development version of the marginaleffects package
# to illustrate the idea
library(remotes)
# remotes::install_github("vincentarelbundock/marginaleffects")
library(marginaleffects)

softmax <- function(x) exp(x) / sum(exp(x)) # softmax function = inverse link function for multinomial (with sum(exp(x)) taken over all outcomes per row)

system.time(preds_latent_backtransformed <- predictions(fit_mblogit, 
                                                        newdata = datagrid(x=unique(dat$x)),
                                                        type = "link") |>  # this is a prediction type that normalizes the log(odds) of the references category to be zero & then drops that category
              # transform(conf.low = predicted - 1.96 * std.error,
              #           conf.high = predicted + 1.96 * std.error) |>
              group_by(rowid) |>
              mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(c(0,x))[-1]) |>
              dplyr::filter(x == 400)) # 0.04s
as.data.frame(preds_latent_backtransformed)
#   rowid type group predicted  std.error statistic       p.value  conf.low conf.high count   x  y
# 1   400 link    y1 0.1177451 0.06627675 -30.38712 8.129214e-203 0.1049064 0.1319233     5 400 y1
# this is correct, matches binomial GLM emmeans result above & 
# avoids having to use the delta method & so is much faster!


preds_link_multinom <- predictions(fit_multinom, 
                                     newdata = datagrid(x=unique(dat$x)),
                                     type = "link") # CORRECT
preds_link_multinom[preds_link_multinom$x==400,]
#       rowid type       group  predicted  std.error  statistic      p.value  conf.low conf.high count   x  y
#   400   400  link         y1  -2.013957 0.06627671  -30.3871 8.134077e-203 -2.143857 -1.884057     5 400 y1

preds_link_mblogit <- predictions(fit_mblogit, 
                                     newdata = datagrid(x=unique(dat$x)),
                                     type = "link")
preds_link_mblogit[preds_link_mblogit$x==400,] 

# CORRECT
# rowid type group predicted  std.error statistic       p.value  conf.low conf.high count   x  y
# 400   400 link    y1 -2.013959 0.06627675 -30.38712 8.129214e-203 -2.143859 -1.884059     5 400 y1

preds_latent_multinom <- predictions(fit_multinom, 
                                   newdata = datagrid(x=unique(dat$x)),
                                   type = "latent") %>% 
  transform(conf.low = predicted - 1.96 * std.error,
            conf.high = predicted + 1.96 * std.error)
preds_latent_multinom[preds_latent_multinom$x==400,] # CORRECT, except columns statistic, p.value, conf.low & conf.high do not show
#      rowid   type group predicted  std.error count   x  y
# 400    400 latent    y2  1.006978 0.03313836     5 400 y1
# 1400   400 latent    y1 -1.006978 0.03313836     5 400 y1

preds_latent_mblogit <- predictions(fit_mblogit, 
                                  newdata = datagrid(x=unique(dat$x)),
                                  type = "latent")
preds_latent_mblogit[preds_latent_mblogit$x==400,] # correct
# rowid   type group predicted  std.error statistic       p.value   conf.low  conf.high count   x
# 400    400 latent    y2   1.00698 0.03313838  30.38712 8.129214e-203  0.9420297  1.0719297     5 400
# 1400   400 latent    y1  -1.00698 0.03313838 -30.38712 8.129214e-203 -1.0719297 -0.9420297     5 400
# y
# 400  y1
# 1400 y1


preds_response_multinom <- predictions(fit_multinom, 
                                     newdata = datagrid(x=unique(dat$x)),
                                     type = "probs") %>% 
  transform(conf.low = predicted - 1.96 * std.error,
            conf.high = predicted + 1.96 * std.error)
preds_response_multinom[preds_response_multinom$x==400,] # CORRECT, except columns statistic, p.value, conf.low & conf.high do not show
# rowid  type group predicted   std.error       count   x  y
#   400   400 probs    y1 0.1177453 0.006660415     5 400 y1

preds_response_mblogit <- predictions(fit_mblogit, 
                                    newdata = datagrid(x=unique(dat$x)),
                                    type = "response")
preds_response_mblogit[preds_response_mblogit$x==400,]
# rowid     type group predicted   std.error statistic      p.value  conf.low conf.high count   x
# 400    400 response    y2 0.8822549 0.006660406 132.46263 0.000000e+00 0.8692008 0.8953091     5 400
# 1400   400 response    y1 0.1177451 0.006660406  17.67836 6.156201e-70 0.1046909 0.1307992     5 400
# y
# 400  y1
# 1400 y1





system.time(preds_link_backtransformed_multinom <- preds_link_multinom |>  # this is a prediction type that normalizes the log(odds) of the references category to be zero & then drops that category
              # transform(conf.low = predicted - 1.96 * std.error,
              #           conf.high = predicted + 1.96 * std.error) |>
              group_by(rowid) |>
              mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(c(0,x))[-1]) |>
              dplyr::filter(x == 400)) # 0.04s


link_marginaleffects = predictions(fit_mblogit, 
                                   newdata = datagrid(x=unique(dat$x)),
                                   type = "link")
link_marginaleffects[link_marginaleffects$x==400,]

link_emmeans = data.frame(as.data.frame(emmeans(fit_mblogit, ~ y, by="x", at=list(x=unique(dat$x)), 
                                  mode="latent", df=Inf), type="link"))
link_emmeans[link_emmeans$x==400,]
softmax(c(0,-1.00698)*2) # 0.117745 # this is correct, but check *2 factor & check if this is correct for >2 groups
softmax(c(0,-1.07193)*2) # 0.1049064
softmax(c(0,-0.9420297)*2) # 0.1319233

# QUESTION: would something like this be possible also with emmeans & allow for large
# performance gains?
# (type="link" with behaviour as in mclogit::predict.mblogit, i.e. with logits of reference
# level coded as zero, is currently not available in emmeans, so I couldn't readily try)

# if X is the model matrix, predictions on that link scale are just given by
X <- model.matrix(fit_multinom)
betahat <- t(rbind(0, coef(fit_multinom ))) # model coefficients, with expicit zero row added for reference category & transposed
preds_link <- X %*% betahat # predictions on the link scale (with explicit zero for reference level included here, in mclogit::predict.mblogit this level/column is dropped)
colnames(preds_link) <- fit_multinom $lev
preds_link

# NOTE: using mode="latent" and type="response" in the summary/as.data.frame
# produces a column with a backtransformation e^y - this is not correct -
# the correct backtransformation should be the softmax function (applied in a grouped way)
head(data.frame(as.data.frame(emmeans(fit_mblogit, ~ y, by="x", at=list(x=unique(dat$x)), 
                                 mode="latent", df=Inf), type="response"), check.names=F))
# y x          e^y           SE  df    asymp.LCL    asymp.UCL
# 1 y2 1 1.374146e+02 1.683433e+01 Inf 1.080823e+02 1.747075e+02
# 2 y1 1 7.277245e-03 8.915173e-04 Inf 5.723852e-03 9.252212e-03
# 3 y2 2 1.360726e+02 1.663777e+01 Inf 1.070762e+02 1.729212e+02
# 4 y1 2 7.349019e-03 8.985741e-04 Inf 5.782982e-03 9.339141e-03
# 5 y2 3 1.347436e+02 1.644345e+01 Inf 1.060796e+02 1.711530e+02
# 6 y1 3 7.421502e-03 9.056835e-04 Inf 5.842724e-03 9.426886e-03

# NOTE: minor detail: I noticed that df are taken into account differently in 
# mclogit::mblogit & nnet::multinom models - maybe it could be good to harmonize this?
# i.e. change line 111 in https://github.com/rvlenth/emmeans/blob/master/R/multinom-support.R
# to
# object$edf = object$model.df

