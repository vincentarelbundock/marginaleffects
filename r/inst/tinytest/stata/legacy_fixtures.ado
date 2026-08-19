*! marginaleffects cross-software fixtures for the estimators that used to live
*! in the frozen `stata.rds` blob.
*! Run from Stata with: legacy_fixtures, resultsdir("results")
*!
*! `stata.rds` was built by estimate.do + clean.R, a chain that no longer runs:
*! estimate.do begins by cd-ing into tests/testthat/stata, which disappeared
*! when the package moved under r/, and it writes .xls through the user-written
*! `outreg2` while clean.R reads .txt. The numbers in the blob are therefore
*! unreproducible. This file regenerates the comparisons that the newer
*! misc/args/models suites do not already cover -- panel, instrumental
*! variables, stratified Cox, right-censored tobit, and interactions -- on the
*! same shared `mfx_*` helpers, from datasets pulled over the network so both
*! sides read identical rows.

capture program drop legacy_fixtures
program define legacy_fixtures
    version 17.0
    syntax [, RESULTSDIR(string)]

    if `"`resultsdir'"' == "" local resultsdir "results"
    capture mkdir `"`resultsdir'"'

    tempfile combined
    mfx_open `"`combined'"'

    local rd "https://vincentarelbundock.github.io/Rdatasets/csv"

    * ======================================================================
    * 1. Linear panel models
    * ======================================================================
    * Only slopes are exported. After a fixed-effects fit the intercept is not
    * identified, so `margins` reports a predictive margin built from xb alone
    * and the two packages do not agree about what the level means. The
    * derivative is unaffected by that, which is why the legacy fixtures also
    * compared dydx() and nothing else.
    import delimited `"`rd'/plm/Grunfeld.csv"', clear varnames(1)
    xtset firm year

    quietly regress inv value capital
    quietly margins, dydx(value capital) level(95)
    mfx_export, fixture("panel_pooling_slopes")
    quietly margins, level(95)
    mfx_export, fixture("panel_pooling_predictions")

    quietly xtreg inv value capital, fe
    quietly margins, dydx(value capital) level(95)
    mfx_export, fixture("panel_within_slopes")

    quietly xtreg inv value capital, fe vce(cluster firm)
    quietly margins, dydx(value capital) level(95)
    mfx_export, fixture("panel_within_cluster_slopes")

    * Swamy-Arora random effects.
    quietly xtreg inv value capital, sa
    quietly margins, dydx(value capital) level(95)
    mfx_export, fixture("panel_swamy_arora_slopes")

    * Poisson with fixed effects is deliberately absent. Stata conditions the
    * fixed effects out of the likelihood rather than estimating them, so
    * `margins` after `xtpoisson, fe` differentiates exp(xb) alone, while
    * fixest estimates the fixed effects and includes them in the prediction.
    * dY/dX = b * exp(xb + u_i) depends on u_i, so the two differentiate
    * different functions. The linear panel models above are unaffected,
    * because there the derivative is the coefficient itself.

    * ======================================================================
    * 3. Instrumental variables
    * ======================================================================
    * Kmenta's classic supply-and-demand system. `sem/Kmenta` is the same table
    * as `ivreg::Kmenta`, column for column.
    import delimited `"`rd'/sem/Kmenta.csv"', clear varnames(1)

    * `small` applies the n - k divisor that ivreg::ivreg uses. Without it
    * Stata divides by n, which inflates every standard error by
    * sqrt(n / (n - k)) -- 8.5% on these 20 rows.
    quietly ivregress 2sls q d (p = d f a), small
    quietly margins, dydx(p d) level(95)
    mfx_export, fixture("iv_2sls_slopes")
    quietly margins, level(95)
    mfx_export, fixture("iv_2sls_predictions")

    * `small` applies the n / (n - k) correction that estimatr uses by default.
    quietly ivregress 2sls q d (p = d f a), vce(robust) small
    quietly margins, dydx(p d) level(95)
    mfx_export, fixture("iv_2sls_robust_slopes")

    * ======================================================================
    * 4. Stratified Cox
    * ======================================================================
    * misc.ado covers an unstratified Cox; a stratum-specific baseline hazard
    * is a separate code path on both sides.
    import delimited `"`rd'/KMsurv/kidtran.csv"', clear varnames(1)
    stset time, failure(delta)
    quietly stcox c.age, strata(gender)
    quietly margins, dydx(age) predict(xb) level(95)
    mfx_export, fixture("cox_strata_slopes")

    * ======================================================================
    * 5. Two-limit tobit
    * ======================================================================
    * misc.ado covers left censoring only.
    import delimited `"`rd'/AER/Affairs.csv"', clear varnames(1)
    quietly tobit affairs age yearsmarried religiousness occupation rating, ll(0) ul(4)
    quietly margins, dydx(age yearsmarried religiousness occupation rating) level(95)
    mfx_export, fixture("tobit_twolimit_slopes")
    quietly margins, level(95)
    mfx_export, fixture("tobit_twolimit_predictions")

    * ======================================================================
    * 6. Interactions
    * ======================================================================
    * A slope which depends on another regressor: the chain rule has a second
    * term, and averaging it is not the same as evaluating it at the mean.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)

    quietly regress mpg c.hp##c.wt
    quietly margins, dydx(hp wt) level(95)
    mfx_export, fixture("interaction_lm_slopes")

    quietly logit am c.hp##c.wt
    quietly margins, dydx(hp wt) level(95)
    mfx_export, fixture("interaction_logit_slopes")

    quietly qreg mpg c.hp##c.wt i.cyl
    quietly margins, dydx(hp wt) level(95)
    mfx_export, fixture("interaction_qreg_slopes")

    mfx_close `"`combined'"' `"`resultsdir'/legacy.csv"'

    display as result "marginaleffects legacy fixtures written to `resultsdir'"
end
