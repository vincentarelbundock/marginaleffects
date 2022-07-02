
#' internal function to build the type dictionary
#'
#' @noRd
type_dictionary_build <- function() {
text <-
'class,base,insight,marginaleffects,predictions
other,class,classification,FALSE,TRUE
other,response,expectation,TRUE,TRUE
other,link,link,TRUE,TRUE
bam,response,expectation,TRUE,TRUE
bam,link,link,TRUE,TRUE
betareg,response,expectation,TRUE,TRUE
betareg,link,link,TRUE,TRUE
betareg,precision,NA,TRUE,TRUE
betareg,quantile,NA,TRUE,TRUE
betareg,variance,NA,TRUE,TRUE
bife,response,expectation,TRUE,TRUE
bife,link,link,TRUE,TRUE
bracl,probs,expectation,TRUE,TRUE
brglmFit,link,link,TRUE,TRUE
brglmFit,response,expectation,TRUE,TRUE
brmsfit,response,expectation,TRUE,TRUE
brmsfit,link,link,TRUE,TRUE
brmsfit,prediction,prediction,TRUE,TRUE
brmsfit,average,NA,TRUE,TRUE
brmultinom,class,classification,FALSE,TRUE
brmultinom,probs,expectation,TRUE,TRUE
clm,prob,response,TRUE,TRUE
clm,cum.prob,NA,TRUE,TRUE
clm,linear_predictor,NA,TRUE,TRUE
clm,class,classification,FALSE,TRUE
clogit,lp,NA,TRUE,TRUE
clogit,expected,NA,TRUE,TRUE
clogit,risk,NA,TRUE,TRUE
clogit,survival,NA,TRUE,TRUE
coxph,expected,NA,TRUE,TRUE
coxph,lp,NA,TRUE,TRUE
coxph,risk,NA,TRUE,TRUE
coxph,survival,NA,TRUE,TRUE
crch,response,expectation,FALSE,TRUE
crch,location,NA,TRUE,TRUE
crch,scale,NA,TRUE,TRUE
crch,density,NA,TRUE,TRUE
hxlr,cumprob,NA,TRUE,TRUE
hxlr,location,expectation,TRUE,TRUE
hxlr,scale,NA,TRUE,TRUE
hxlr,density,NA,TRUE,TRUE
fixest,link,link,TRUE,TRUE
fixest,response,expectation,TRUE,TRUE
hurdle,response,expectation,TRUE,TRUE
hurdle,prob,NA,TRUE,TRUE
hurdle,count,NA,TRUE,TRUE
hurdle,zero,NA,TRUE,TRUE
iv_robust,response,expectation,TRUE,TRUE
lm,response,expectation,TRUE,TRUE
gam,response,expectation,TRUE,TRUE
gam,link,link,TRUE,TRUE
Gam,response,expectation,TRUE,TRUE
Gam,link,link,TRUE,TRUE
geeglm,response,expectation,TRUE,TRUE
geeglm,link,link,TRUE,TRUE
glimML,response,expectation,TRUE,TRUE
glimML,link,link,TRUE,TRUE
glm,response,expectation,TRUE,TRUE
glm,link,link,TRUE,TRUE
glmerMod,response,expectation,TRUE,TRUE
glmerMod,link,link,TRUE,TRUE
glmrob,response,expectation,TRUE,TRUE
glmrob,link,link,TRUE,TRUE
glmmTMB,response,expectation,TRUE,TRUE
glmmTMB,link,link,TRUE,TRUE
glmmTMB,conditional,NA,TRUE,TRUE
glmmTMB,zprob,NA,TRUE,TRUE
glmmTMB,zlink,NA,TRUE,TRUE
glmmTMB,disp,NA,TRUE,TRUE
glmmPQL,link,link,TRUE,TRUE
glmmPQL,response,expectation,TRUE,TRUE
glmx,response,expectation,TRUE,TRUE
ivreg,response,expectation,TRUE,TRUE
lmerMod,response,expectation,TRUE,TRUE
lmerModLmerTest,response,expectation,TRUE,TRUE
lmrob,response,expectation,TRUE,TRUE
lm_robust,response,expectation,TRUE,TRUE
mblogit,response,expectation,TRUE,TRUE
mblogit,link,link,TRUE,TRUE
mclogit,link,link,TRUE,TRUE
mclogit,response,expectation,FALSE,TRUE
multinom,probs,expectation,TRUE,TRUE
mhurdle,E,NA,TRUE,TRUE
mhurdle,Ep,NA,TRUE,TRUE
mhurdle,p,NA,TRUE,TRUE
mlogit,response,NA,TRUE,TRUE
negbin,link,link,TRUE,TRUE
negbin,response,expectation,TRUE,TRUE
polr,probs,expectation,TRUE,TRUE
rlm,response,expectation,TRUE,TRUE
selection,response,NA,TRUE,TRUE
selection,link,NA,TRUE,TRUE
selection,unconditional,NA,TRUE,TRUE
speedlm,response,expectation,TRUE,TRUE
speedglm,link,link,TRUE,TRUE
speedglm,response,expectation,TRUE,TRUE
stanreg,link,link,TRUE,TRUE
stanreg,response,expectation,TRUE,TRUE
svyglm,response,expectation,TRUE,TRUE
svyglm,link,link,TRUE,TRUE
tobit,response,expectation,TRUE,TRUE
tobit1,linpred,NA,TRUE,TRUE
tobit1,expvalue,NA,TRUE,TRUE
tobit1,prob,NA,TRUE,TRUE
zeroinfl,response,expectation,TRUE,TRUE
zeroinfl,prob,NA,TRUE,TRUE
zeroinfl,count,NA,TRUE,TRUE
zeroinfl,zero,NA,TRUE,TRUE'
out <- utils::read.csv(
    text = text,
    colClasses = c("character", "character", "character", "logical", "logical"))
for (i in 1:2) {
    out[[i]] <- trimws(out[[i]])
}
return(out)
}


#' type dictionary
#'
#' insight::get_predict accepts a `predict` argument
#' stats::predict accepts a `type` argument
#' this dictionary converts
#' @noRd
type_dictionary <- type_dictionary_build()
