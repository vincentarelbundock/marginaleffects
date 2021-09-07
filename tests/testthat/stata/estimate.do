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


* ivreg::ivreg
clear
use data/ivreg_ivreg_01.dta
quiet ivregress 2sls Q D (P = D F A)
quiet margins, dydx(P D) post
outreg2 using "results/ivreg_ivreg_01.xls", dec(10) excel replace noaster sideway noparen stats(coef se)
