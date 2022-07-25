cd ~/repos/marginaleffects/tests/testthat/stata


* stats::glm
clear
use databases/stats_glm_01.dta
quiet logit y c.x1##c.x2
quiet margins, dydx(x1 x2) post
outreg2 using "results/stats_glm_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* stats::lm
clear
use databases/stats_lm_01.dta
quiet reg y c.x1##c.x2
quiet margins, dydx(x1 x2) post
outreg2 using "results/stats_lm_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* survival::coxph
clear
use databases/survival_coxph_01.dta
stset time status
quiet stcox x, strata(sex) nohr
quiet margins, dydx(x) exp(predict(xb)) post
outreg2 using "results/survival_coxph_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* betareg::betareg
clear
use databases/betareg_betareg_01.dta
quiet betareg yield i.batch temp
quiet margins, dydx(temp) post
outreg2 using "results/betareg_betareg_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* pscl::zeroinfl
clear
use databases/pscl_bioChemists.dta
quiet zinb art kid phd, inflate(ment)
quiet margins, dydx(*) post
outreg2 using "results/pscl_zeroinfl_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* MASS::polr
clear
use databases/MASS_polr_01.dta
quiet ologit y x1 x2
quiet margins, dydx(x1 x2) post
outreg2 using "results/MASS_polr_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* nnet::multinom
clear
use databases/MASS_polr_01.dta
quiet mlogit y x1 x2, baseoutcome(1)
quiet margins, dydx(*) post
outreg2 using "results/nnet_multinom_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* fixest::feols
clear
use databases/plm_emplUK.dta
xtset firm year
quiet xtreg wage c.capital##c.output, fe cluster(firm)
quiet margins, dydx(*) post
outreg2 using "results/fixest_feols_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

gen logwage = log(wage)
quiet xtpoisson logwage c.capital##c.output, fe vce(robust)
quiet margins, dydx(*) post
outreg2 using "results/fixest_fepois_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* ivreg::ivreg
clear
use databases/ivreg_ivreg_01.dta
quiet ivregress 2sls Q D (P = D F A)
quiet margins, dydx(P D) post
outreg2 using "results/ivreg_ivreg_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* MASS::glm.nb
clear
use databases/mtcars.dta
quiet nbreg carb wt i.cyl
quiet margins, dydx(*) post
outreg2 using "results/MASS_glm_nb.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* AER::tobit
clear
use databases/affairs.dta
quiet tobit affairs age yearsmarried religiousness occupation rating, ll(0)
quiet margins, dydx(*) post
outreg2 using "results/AER_tobit.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* AER::tobit (right-censured)
clear
use databases/affairs.dta
quiet tobit affairs age yearsmarried religiousness occupation rating, ul(4) ll(0)
quiet margins, dydx(*) post
outreg2 using "results/AER_tobit_right4.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* estimatr::lm_robust
clear
use databases/mtcars.dta
quiet reg carb wt i.cyl, vce(hc2)
quiet margins, dydx(*) post
outreg2 using "results/estimatr_lm_robust.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* estimatr::iv_robust
clear 
use databases/kmenta.dta
quiet ivregress 2sls Q D (P = D F A), vce(robust) small
quiet margins, dydx(*) post
outreg2 using "results/estimatr_iv_robust.xls", dec(10) excel replace noaster sideway

* lmer4
clear
use databases/lme4_01.dta
xtset clus

quiet xtreg y c.x1##c.x2, re
quiet margins, dydx(*) post
outreg2 using "results/lme4_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

clear
use databases/lme4_02.dta
xtset clus

quiet xtlogit y c.x1##c.x2, re
quiet margins, dydx(*) post
outreg2 using "results/lme4_02.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* quantreg::rq
clear
use databases/mtcars.dta
quiet qreg mpg c.hp##c.wt i.cyl
quiet margins, dydx(*) post
outreg2 using "results/quantreg_rq_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* truncreg::truncreg
clear
use databases/tobin.dta
quiet truncreg durable age quant, ll(0)
quiet margins, dydx(*) post
outreg2 using "results/truncreg_truncreg_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


******************* PANEL

* emplUK
clear
use databases/plm_emplUK.dta
xtset firm year
gen logwage = log(wage)

* fixest::feols
quiet xtreg wage c.capital##c.output, fe cluster(firm)
quiet margins, dydx(*) post
outreg2 using "results/fixest_feols_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* fixest::fepois
quiet xtpoisson logwage c.capital##c.output, fe vce(robust)
quiet margins, dydx(*) post
outreg2 using "results/fixest_fepois_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* Grunfeld
clear
use databases/plm_Grunfeld.dta
xtset firm year

* plm pooling
quiet reg inv c.value##c.capital
quiet margins, dydx(*) post
outreg2 using "results/plm_pooling_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* plm Swamy-Arora
quiet xtreg inv c.value##c.capital, sa
quiet margins, dydx(*) post
outreg2 using "results/plm_sa_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


******************* ELASTICITY

* stats::glm
clear
use databases/stats_glm_01.dta

quiet logit y c.x1##c.x2
quiet margins, eyex(x1 x2) post
outreg2 using "results/stats_glm_elasticity_eyex.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

quiet logit y c.x1##c.x2
quiet margins, dyex(x1 x2) post
outreg2 using "results/stats_glm_elasticity_dyex.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

quiet logit y c.x1##c.x2
quiet margins, eydx(x1 x2) post
outreg2 using "results/stats_glm_elasticity_eydx.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* stats::lm
clear
use databases/stats_lm_01.dta

quiet reg y c.x1##c.x2
quiet margins, eyex(x1 x2) post
outreg2 using "results/stats_lm_elasticity_eyex.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

quiet reg y c.x1##c.x2
quiet margins, dyex(x1 x2) post
outreg2 using "results/stats_lm_elasticity_dyex.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

quiet reg y c.x1##c.x2
quiet margins, eydx(x1 x2) post
outreg2 using "results/stats_lm_elasticity_eydx.xls", dec(10) excel replace noaster sideway noparen stats(coef se)
