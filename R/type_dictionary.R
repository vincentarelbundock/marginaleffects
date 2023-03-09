
#' internal function to build the type dictionary
#'
#' @noRd
type_dictionary_build <- function() {
text <-
'class,type
other,response
other,class
other,link
bam,response
bam,link
betareg,response
betareg,link
betareg,precision
betareg,quantile
betareg,variance
bife,response
bife,link
bracl,probs
brglmFit,response
brglmFit,link
brmsfit,response
brmsfit,link
brmsfit,prediction
brmsfit,average
brmultinom,probs
brmultinom,class
clm,prob
clm,cum.prob
clm,linear.predictor
clogit,expected
clogit,lp
clogit,risk
clogit,survival
coxph,expected
coxph,lp
coxph,risk
coxph,survival
crch,response
crch,location
crch,scale
crch,density
hetprob,pr
hetprob,xb
hxlr,location
hxlr,cumprob
hxlr,scale
hxlr,density
ivpml,pr
ivpml,xb
fixest,response
fixest,link
hurdle,response
hurdle,prob
hurdle,count
hurdle,zero
iv_robust,response
lm,response
gam,response
gam,link
Gam,response
Gam,link
geeglm,response
geeglm,link
glimML,response
glimML,link
glm,response
glm,link
glmerMod,response
glmerMod,link
glmrob,response
glmrob,link
glmmTMB,response
glmmTMB,link
glmmTMB,conditional
glmmTMB,zprob
glmmTMB,zlink
glmmTMB,disp
glmmPQL,response
glmmPQL,link
glmx,response
ivreg,response
lmerMod,response
lmerModLmerTest,response
lmrob,response
lm_robust,response
lrm,fitted
lrm,lp
lrm,mean
mblogit,response
mblogit,latent
mblogit,link
mclogit,response
mclogit,latent
mclogit,link
MCMCglmm,response
multinom,probs
multinom,latent
mhurdle,E
mhurdle,Ep
mhurdle,p
mlogit,response
negbin,response
negbin,link
ols,lp
orm,fitted
orm,mean
orm,lp
polr,probs
rlm,response
selection,response
selection,link
selection,unconditional
speedlm,response
speedglm,response
speedglm,link
stanreg,response
stanreg,link
svyglm,response
svyglm,link
tobit,response
tobit1,expvalue
tobit1,linpred
tobit1,prob
zeroinfl,response
zeroinfl,prob
zeroinfl,count
zeroinfl,zero'
out <- utils::read.csv(
    text = text,
    colClasses = c("character", "character"))
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
