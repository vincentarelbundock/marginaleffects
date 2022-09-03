library(nnet)
library(mclogit)
library(emmeans)
library(dplyr)
library(tidyverse)

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

# naive symmeric Delta method CIs on response scale
data.frame(predictions(fit1_multinom, 
                       newdata = newdata,
                       type = "probs")) %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error)
# rowid     type group predicted std.error mpg cyl am    conf.low conf.high
# 1     1 probs        0   0.63974 0.2319219  21   4  1  0.18517307 1.0943069
# 2     1 probs        1   0.36026 0.2319219  21   4  1 -0.09430687 0.8148269

# NOTE there is a bug in the default type="preds" multinom.predict method
# that sometimes causes it to drop the reference level, like here:
nnet:::predict.multinom(fit1_multinom, newdata=newdata, type="probs") # mistake in predict.multinom
# NOT CORRECT
# 1 
# 0.36026
# should return both outcome levels



data.frame(predictions(fit1_mblogit, 
                       newdata = newdata,
                       type = "response"))
# CIs are included here because mblogit inherits lm & insight treats it as a lm
# rowid     type group predicted std.error statistic     p.value    conf.low conf.high mpg cyl am
# 1     1 response     0 0.6396388  0.231941  2.757765 0.005819794  0.18504283 1.0942347  21   4  1
# 2     1 response     1 0.3603612  0.231941  1.553677 0.120261562 -0.09423467 0.8149572  21   4  1


# CIs that match emmeans result of binomial GLM, here obtained via
# backtransformation from link scale for multinomial models fit1_mblogit & fit1_multi
# (reference level now dropped but equal to 1-(other level))
data.frame(predictions(fit1_mblogit, 
            newdata = newdata,
            type = "link") %>%
  # transform(conf.low = predicted - 1.96 * std.error,
  #           conf.high = predicted + 1.96 * std.error) |>
  group_by(rowid) |>
  mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(c(0,x))[-1]))
#   rowid type group predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1 link     1 0.3603612  1.007013 -0.5698008 0.5688128 0.07259429 0.8021699  21   4  1


data.frame(predictions(fit1_multinom, 
                       newdata = newdata,
                       type = "link") %>%
             # transform(conf.low = predicted - 1.96 * std.error,
             #           conf.high = predicted + 1.96 * std.error) |>
             group_by(rowid) |>
             mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(c(0,x))[-1]))
#   rowid type group predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1 link     1   0.36026  1.007054 -0.5702135 0.5685329 0.07255927 0.8021131  21   4  1

# predictions on latent (centered logit) scale
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
            type = "latent")
# rowid   type group  predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1 latent     0  0.2868983 0.5035063  0.5698008 0.5688128 -0.6999558 1.2737524  21   4  1
# 2     1 latent     1 -0.2868983 0.5035063 -0.5698008 0.5688128 -1.2737524 0.6999558  21   4  1

predictions(fit1_multinom, 
            newdata = newdata,
            type = "latent") %>%
  transform(conf.low = predicted - 1.96 * std.error,
            conf.high = predicted + 1.96 * std.error)
# rowid   type group  predicted std.error mpg cyl am  conf.low conf.high
# 1     1 latent     0  0.2871179  0.503527  21   4  1 -0.699795  1.274031
# 2     1 latent     1 -0.2871179  0.503527  21   4  1 -1.274031  0.699795

# note: CIs backtransformed from latent scale to response scale that match
# those of the emmeans binomial GLM can be obtained here using
data.frame(predictions(fit1_mblogit, 
                       newdata = newdata,
                       type = "latent") %>%
             # transform(conf.low = predicted - 1.96 * std.error,
             #           conf.high = predicted + 1.96 * std.error) |>
             group_by(rowid) |>
             mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(c(0,x[-1])*2)))
#   rowid   type group predicted std.error  statistic   p.value   conf.low conf.high mpg cyl am
# 1     1 latent     0 0.6396388 0.5035063  0.5698008 0.5688128 0.92740571 0.1978301  21   4  1
# 2     1 latent     1 0.3603612 0.5035063 -0.5698008 0.5688128 0.07259429 0.8021699  21   4  1


# TESTS WITH MULTINOMIAL OUTCOMES & IN-FORMULA TRANSFORMATION (spline) ####

newdata = data.frame(mpg = c(21), am = factor(c(1), levels=levels(dat$am))) # with single row
# newdata = data.frame(mpg = c(21, 22), am = factor(c(0, 1), levels=levels(dat$am))) 
# with multiple rows everything also works as expected

library(splines)
fit2_multinom = multinom(cyl ~ ns(mpg, df=2) + am, data = dat)

# emmeans
data.frame(emmeans(fit2_multinom, ~ cyl|mpg+am, at = newdata, mode = "prob", df=Inf)) 
#   cyl mpg am       prob         SE  df   asymp.LCL asymp.UCL
# 1   4  21  1 0.24255335 0.24418315 Inf -0.23603683 0.7211435
# 2   6  21  1 0.73579464 0.24466481 Inf  0.25626043 1.2153289
# 3   8  21  1 0.02165201 0.06043248 Inf -0.09679348 0.1400975

# marginaleffects
# naive symmeric Delta method CIs on response scale
data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "probs")) %>%
  transform(conf.low = predicted - 1.96 * std.error,
            conf.high = predicted + 1.96 * std.error)
# rowid     type group  predicted  std.error mpg am cyl    conf.low conf.high
# 1     1 probs      4 0.24255335 0.24423231  21  1   6 -0.23614197 0.7212487
# 2     1 probs      6 0.73579464 0.24472846  21  1   6  0.25612686 1.2154624
# 3     1 probs      8 0.02165201 0.06043029  21  1   6 -0.09679136 0.1400954


# better CIs backtransformed from link scale
# (reference level now dropped but equal to 1-(sum of the rest))
data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "link") %>%
             # transform(conf.low = predicted - 1.96 * std.error,
             #           conf.high = predicted + 1.96 * std.error) |>
             group_by(rowid) |>
             mutate_at(c("predicted", "conf.low", "conf.high"), function (x) softmax(c(0,x))[-1]))
# rowid type group  predicted std.error  statistic   p.value     conf.low conf.high mpg am cyl
# 1     1 link     6 0.73579464  1.331576  0.8333956 0.4046217 0.1823768061 0.5206336  21  1   6
# 2     1 link     8 0.02165201  3.074765 -0.7857914 0.4319897 0.0001761596 0.4667440  21  1   6


# predictions on latent (centered logit scale)
data.frame(predictions(fit2_multinom, 
                       newdata = newdata,
                       type = "latent") %>%
             transform(conf.low = predicted - 1.96 * std.error,
                       conf.high = predicted + 1.96 * std.error))
# rowid   type group  predicted std.error mpg am cyl   conf.low conf.high
# 1     1 latent     4  0.4354647  1.260945  21  1   6 -2.0359866  2.906916
# 2     1 latent     6  1.5451941  1.074008  21  1   6 -0.5598611  3.650249
# 3     1 latent     8 -1.9806588  1.927136  21  1   6 -5.7578455  1.796528
