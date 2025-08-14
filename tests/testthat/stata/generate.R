library(haven)
library(ivreg)
library(betareg)

################
#  stats::glm  #
################
set.seed(1024)
N <- 100
dat <- data.frame(x1 = rnorm(N), x2 = rnorm(N))
dat$y <- plogis(dat$x1 + dat$x2 + dat$x1 * dat$x2)
dat$y <- rbinom(N, 1, dat$y)
haven::write_dta(dat, path = "data/stats_glm_01.dta")


###############
#  stats::lm  #
###############
set.seed(1024)
N <- 100
dat <- data.frame(x1 = rnorm(N), x2 = rnorm(N))
dat$y <- dat$x1 + dat$x2 + dat$x1 * dat$x2 + rnorm(N)
haven::write_dta(dat, path = "data/stats_lm_01.dta")


################
#  MASS::polr  #
################
set.seed(1024)
N <- 1000
dat <- data.frame(x1 = rnorm(N), x2 = rnorm(N))
dat$y <- dat$x1 + dat$x2 + dat$x1 * dat$x2 + rnorm(N)
dat$y <- cut(dat$y, breaks = 4)
dat$y <- factor(as.numeric(dat$y))
haven::write_dta(dat, path = "data/MASS_polr_01.dta")


##########
#  lme4  #
##########

set.seed(1024)
N <- 1000

dat_glm <- data.frame(x1 = rnorm(N),
                      x2 = rnorm(N),
                      clus = sample(letters[1:10], N, replace = TRUE))
dat_glm$clus <- as.factor(dat_glm$clus)
dat_glm$y <- dat_glm$x1 + dat_glm$x2 + dat_glm$x1 * dat_glm$x2 + .1 * as.numeric(dat_glm$clus)
dat_glm$y <- rbinom(N, 1, plogis(dat_glm$y))

dat_lm <- data.frame(x1 = rnorm(N),
                     x2 = rnorm(N),
                     clus = sample(letters[1:10], N, replace = TRUE))
dat_lm$clus <- as.factor(dat_lm$clus)
dat_lm$y <- dat_lm$x1 + dat_lm$x2 + dat_lm$x1 * dat_lm$x2 + .1 * as.numeric(dat_lm$clus) + rnorm(N)

haven::write_dta(dat_lm, test_path("stata/data/lme4_01.dta"))
haven::write_dta(dat_glm, test_path("stata/data/lme4_02.dta"))


############
#  fixest  #
############
data(trade, package = "fixest")
haven::write_dta(trade, path = test_path("stata/data/fixest_trade_01.dta"))


#########
#  plm  #
#########
data(EmplUK, package = "plm")
haven::write_dta(EmplUK, path = test_path("stata/data/plm_emplUK.dta"))

data(Grunfeld, package = "plm")
haven::write_dta(Grunfeld, path = test_path("stata/data/plm_Grunfeld.dta"))

##################
#  ivreg::ivreg  #
##################
haven::write_dta(ivreg::Kmenta, path = "data/ivreg_ivreg_01.dta")


######################
#  betareg::betareg  #
######################
data("GasolineYield", package = "betareg")
haven::write_dta(GasolineYield, path = "data/betareg_betareg_01.dta")


####################
#  fixest::fixest  #
####################
haven::write_dta(mtcars, path = "data/fixest_fixest_01.dta")


##################
#  MASS::glm.nb  #
##################
haven::write_dta(mtcars, path = "data/mtcars.dta")


################
#  AER::tobit  #
################
write.table(data(Affairs, package = "AER"))
haven::write_dta(Affairs, path = "data/affairs.dta")


#########################
#  estimatr::lm_robust  #
#########################
haven::write_dta(mtcars, path = "data/mtcars.dta")


#########################
#  estimatr::iv_robust  #
#########################
write.table(data(Kmenta, package = "ivreg"))
haven::write_dta(Kmenta, path = "data/kmenta.dta")


##################
#  quantreg::rq  #
##################
haven::write_dta(mtcars, path = "data/mtcars.dta")

########################
#  truncreg::truncreg  #
########################
write.table(data(tobin, package = "survival"))
haven::write_dta(tobin, path = "data/tobin.dta")

