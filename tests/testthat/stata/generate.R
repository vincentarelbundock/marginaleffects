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
