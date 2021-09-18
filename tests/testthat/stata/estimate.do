cd ~/repos/marginaleffects/tests/testthat/stata


* stats::glm
clear
use data/stats_glm_01.dta
quiet logit y c.x1##c.x2
quiet margins, dydx(x1 x2) post
outreg2 using "results/stats_glm_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* stats::lm
clear
use data/stats_lm_01.dta
quiet reg y c.x1##c.x2
quiet margins, dydx(x1 x2) post
outreg2 using "results/stats_lm_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* betareg::betareg
clear
use data/betareg_betareg_01.dta
quiet betareg yield i.batch temp
quiet margins, dydx(temp) post
outreg2 using "results/betareg_betareg_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* MASS::polr
clear
use data/MASS_polr_01.dta
quiet ologit y x1 x2
quiet margins, dydx(x1 x2) post
outreg2 using "results/MASS_polr_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* fixest::feols
clear
use data/plm_emplUK.dta
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
use data/ivreg_ivreg_01.dta
quiet ivregress 2sls Q D (P = D F A)
quiet margins, dydx(P D) post
outreg2 using "results/ivreg_ivreg_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* MASS::glm.nb
clear
use data/mtcars.dta
quiet nbreg carb wt i.cyl
quiet margins, dydx(*) post
outreg2 using "results/MASS_glm_nb.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* AER::tobit
clear
use data/affairs.dta
quiet tobit affairs age yearsmarried religiousness occupation rating, ll(0)
quiet margins, dydx(*) post
outreg2 using "results/AER_tobit.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* AER::tobit (right-censured)
clear
use data/affairs.dta
quiet tobit affairs age yearsmarried religiousness occupation rating, ul(4) ll(0)
quiet margins, dydx(*) post
outreg2 using "results/AER_tobit_right4.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* estimatr::lm_robust
clear
use data/mtcars.dta
quiet reg carb wt i.cyl, vce(hc2)
quiet margins, dydx(*) post
outreg2 using "results/estimatr_lm_robust.xls", dec(10) excel replace noaster sideway noparen stats(coef se)


* estimatr::iv_robust
clear 
use data/kmenta.dta
quiet ivregress 2sls Q D (P = D F A), vce(robust) small
quiet margins, dydx(*) post
outreg2 using "results/estimatr_iv_robust.xls", dec(10) excel replace noaster sideway


******************* PANEL

* emplUK
clear
use data/plm_emplUK.dta
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
use data/plm_Grunfeld.dta
xtset firm year

* plm pooling
quiet reg inv c.value##c.capital
quiet margins, dydx(*) post
outreg2 using "results/plm_pooling_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)

* plm Swamy-Arora
quiet xtreg inv c.value##c.capital, sa
quiet margins, dydx(*) post
outreg2 using "results/plm_sa_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)
