*! marginaleffects cross-software fixtures from the Rdatasets archive
*! Run from Stata with: misc, resultsdir("results/misc")

capture program drop misc
program define misc
    version 17.0
    syntax [, RESULTSDIR(string)]

    if `"`resultsdir'"' == "" local resultsdir "results/misc"
    capture mkdir `"`resultsdir'"'

    tempfile combined
    postfile mfxpost str244 fixture str244 contrast ///
        double estimate std_error using `"`combined'"', replace

    local rd "https://vincentarelbundock.github.io/Rdatasets/csv"

    * Heckman selection: selection probability and conditional/unconditional outcome.
    import delimited `"`rd'/sampleSelection/Mroz87.csv"', clear varnames(1)
    quietly heckman wage c.educ c.exper, ///
        select(lfp = c.age c.educ kids5 kids618 c.nwifeinc)
    quietly margins, dydx(age educ kids5 kids618 nwifeinc) predict(psel)
    mfx_export using `"`resultsdir'/heckman_selection.csv"'
    quietly margins, dydx(educ exper) predict(ycond)
    mfx_export using `"`resultsdir'/heckman_outcome_conditional.csv"'
    quietly margins, dydx(educ exper) predict(yexpected)
    mfx_export using `"`resultsdir'/heckman_outcome_unconditional.csv"'

    * Zero-inflated negative binomial: overall mean and both equation components.
    import delimited `"`rd'/pscl/bioChemists.csv"', clear varnames(1)
    encode fem, generate(fem_f)
    encode mar, generate(mar_f)
    quietly zinb art c.phd c.ment i.fem_f i.mar_f kid5, inflate(c.ment kid5 i.fem_f)
    quietly margins, dydx(phd ment fem_f mar_f kid5) predict(n)
    mfx_export using `"`resultsdir'/zinb_response.csv"'
    quietly margins, dydx(phd ment fem_f mar_f kid5) expression(exp(predict(xb)))
    mfx_export using `"`resultsdir'/zinb_count_mean.csv"'
    quietly margins, dydx(ment fem_f kid5) predict(pr)
    mfx_export using `"`resultsdir'/zinb_zero_probability.csv"'

    * Positive-count (hurdle count component) model.
    preserve
    keep if art > 0
    quietly tpoisson art c.phd c.ment i.fem_f i.mar_f kid5, ll(0)
    quietly margins, dydx(phd ment fem_f mar_f kid5) predict(n)
    mfx_export using `"`resultsdir'/hurdle_positive_count.csv"'
    restore

    * Binary changes, interactions, by-groups, and weights.
    * Use iris rather than mtcars here: 150 rows give interaction and grouped
    * margins enough support for stable estimation.
    import delimited `"`rd'/datasets/iris.csv"', clear varnames(1)
    generate byte wide = sepalwidth > 3
    generate byte long_sepal = sepallength > 5.8
    generate int scount = round(2 * sepalwidth)
    encode species, generate(species_f)
    quietly logit wide c.petallength##c.petalwidth i.long_sepal, vce(robust)
    quietly margins, dydx(long_sepal)
    mfx_export using `"`resultsdir'/logit_binary.csv"'
    quietly margins long_sepal, over(species_f)
    mfx_export using `"`resultsdir'/logit_binary_by_species.csv"'
    quietly margins, dydx(petallength) at(petalwidth = (0.5 1.5 2.5))
    mfx_export using `"`resultsdir'/logit_interaction_at.csv"'

    generate double aw = _n
    quietly poisson scount c.sepallength##c.petallength i.long_sepal [pw=aw], vce(robust)
    quietly margins, dydx(long_sepal sepallength)
    mfx_export using `"`resultsdir'/poisson_weighted_robust.csv"'

    * Nonlinear transformations and delta-method standard errors.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    quietly logit am c.hp c.wt i.vs
    quietly margins, dydx(hp)
    mfx_export using `"`resultsdir'/logit_dydx.csv"'
    quietly margins vs, post
    quietly nlcom ///
        (ratio: _b[1.vs] / _b[0.vs]) ///
        (lnratio: ln(_b[1.vs] / _b[0.vs])) ///
        (lift: (_b[1.vs] - _b[0.vs]) / _b[0.vs]) ///
        (lnor: ln((_b[1.vs] / (1 - _b[1.vs])) / (_b[0.vs] / (1 - _b[0.vs]))))
    mfx_export using `"`resultsdir'/logit_nonlinear.csv"'

    quietly poisson carb c.hp c.wt
    quietly margins, eyex(hp wt)
    mfx_export using `"`resultsdir'/poisson_eyex.csv"'
    quietly margins, eydx(hp wt)
    mfx_export using `"`resultsdir'/poisson_eydx.csv"'
    quietly margins, dyex(hp wt)
    mfx_export using `"`resultsdir'/poisson_dyex.csv"'

    * A broad model/command matrix. Each fit is exercised with the Stata
    * counterparts of avg_predictions(), avg_comparisons(), and avg_slopes(),
    * both overall and by group.
    quietly regress mpg c.hp c.wt i.am
    mfx_runall "lm" cyl am hp `"`resultsdir'"'

    quietly probit vs c.hp c.wt i.am
    mfx_runall "probit" cyl am hp `"`resultsdir'"'

    quietly cloglog vs c.hp c.wt i.am
    mfx_runall "cloglog" cyl am hp `"`resultsdir'"'

    quietly poisson carb c.hp c.wt i.am
    mfx_runall "poisson" cyl am hp `"`resultsdir'"'

    quietly nbreg carb c.hp c.wt i.am
    mfx_runall "negative_binomial" cyl am hp `"`resultsdir'"'

    quietly glm mpg c.hp c.wt i.am, family(gamma) link(log)
    mfx_runall "gamma_log" cyl am hp `"`resultsdir'"'

    quietly qreg mpg c.hp c.wt i.am
    mfx_runall "quantile_median" cyl am hp `"`resultsdir'"'

    generate double mpg_censored = max(mpg, 18)
    quietly tobit mpg_censored c.hp c.wt i.am, ll(18)
    mfx_runall "tobit_left" cyl am hp `"`resultsdir'"'

    generate double mpg_fraction = mpg / 40
    quietly fracreg logit mpg_fraction c.hp c.wt i.am
    mfx_runall "fractional_logit" cyl am hp `"`resultsdir'"'

    * Representative-value predictions and interaction contrasts exercise
    * newdata/datagrid paths rather than only averaging over observed rows.
    import delimited `"`rd'/datasets/iris.csv"', clear varnames(1)
    generate byte wide = sepalwidth > 3
    generate byte long_sepal = sepallength > 5.8
    quietly logit wide c.petallength##i.long_sepal c.petalwidth
    quietly margins, at(petallength = (1.5 4.5 6.5) long_sepal = (0 1))
    mfx_export using `"`resultsdir'/logit_predictions_at.csv"'
    quietly margins, dydx(long_sepal) at(petallength = (1.5 4.5 6.5))
    mfx_export using `"`resultsdir'/logit_comparisons_at.csv"'

    * Clustered/correlated models. The grouping variable is deterministic so
    * the same Rdatasets CSV can be reconstructed exactly on the R side.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    generate byte panel = mod(_n - 1, 8) + 1
    quietly mixed mpg c.hp c.wt i.am || panel:, mle
    mfx_runall "mixed_gaussian" cyl am hp `"`resultsdir'"'

    xtset panel
    quietly xtgee carb c.hp c.wt i.am, family(poisson) link(log) corr(exchangeable)
    mfx_runall "gee_poisson" cyl am hp `"`resultsdir'"'

    * Ordinal probabilities and categorical changes for every outcome.
    import delimited `"`rd'/MASS/housing.csv"', clear varnames(1)
    generate byte sat_f = 1 if sat == "Low"
    replace sat_f = 2 if sat == "Medium"
    replace sat_f = 3 if sat == "High"
    generate byte infl_f = 1 if infl == "Low"
    replace infl_f = 2 if infl == "Medium"
    replace infl_f = 3 if infl == "High"
    generate byte type_f = 1 if type == "Tower"
    replace type_f = 2 if type == "Apartment"
    replace type_f = 3 if type == "Atrium"
    replace type_f = 4 if type == "Terrace"
    generate byte cont_f = 1 if cont == "Low"
    replace cont_f = 2 if cont == "High"
    expand freq
    quietly ologit sat_f i.infl_f##i.type_f i.cont_f
    quietly margins, predict(outcome(1))
    mfx_export using `"`resultsdir'/ologit_predictions_outcome1.csv"'
    quietly margins, predict(outcome(2))
    mfx_export using `"`resultsdir'/ologit_predictions_outcome2.csv"'
    quietly margins, predict(outcome(3))
    mfx_export using `"`resultsdir'/ologit_predictions_outcome3.csv"'
    forvalues outcome = 1/3 {
        quietly margins, dydx(infl_f type_f cont_f) predict(outcome(`outcome'))
        mfx_export using `"`resultsdir'/ologit_comparisons_outcome`outcome'.csv"'
    }

    * Multinomial probabilities, interactions, and outcome-specific contrasts.
    * Reuse the expanded housing data so the nominal and ordinal fits are
    * directly comparable and avoid quasi-separation in the iris species data.
    quietly mlogit sat_f i.infl_f##i.cont_f i.type_f, baseoutcome(1)
    forvalues outcome = 1/3 {
        quietly margins, predict(outcome(`outcome'))
        mfx_export using `"`resultsdir'/mlogit_predictions_outcome`outcome'.csv"'
        quietly margins, dydx(infl_f cont_f type_f) predict(outcome(`outcome'))
        mfx_export using `"`resultsdir'/mlogit_comparisons_outcome`outcome'.csv"'
    }

    * Survival models. Cox supports LP/relative-hazard margins; parametric
    * Weibull additionally supports mean and median survival-time margins.
    import delimited `"`rd'/KMsurv/kidtran.csv"', clear varnames(1)
    stset time, failure(delta)
    quietly stcox c.age i.gender i.race
    quietly margins, dydx(age gender race) predict(xb)
    mfx_export using `"`resultsdir'/cox_lp.csv"'
    quietly margins, dydx(age gender race) predict(hr)
    mfx_export using `"`resultsdir'/cox_hazard_ratio.csv"'
    quietly streg c.age i.gender i.race, distribution(weibull)
    quietly margins, dydx(age gender race) predict(hr)
    mfx_export using `"`resultsdir'/weibull_hazard_ratio.csv"'
    quietly margins, at(age = (30 50 70)) predict(mean time)
    mfx_export using `"`resultsdir'/weibull_mean_survival.csv"'
    quietly margins, at(age = (30 50 70)) predict(median time)
    mfx_export using `"`resultsdir'/weibull_median_survival.csv"'

    * Survey-weighted/domain margins and by-groups.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    generate long psu = _n
    generate byte strata = mod(_n - 1, 4) + 1
    generate double pwt = mpg
    generate byte domain = cyl != 8
    svyset psu [pweight=pwt], strata(strata) singleunit(centered)
    quietly svy: logit am c.hp i.vs c.wt
    quietly margins, dydx(hp vs) subpop(domain)
    mfx_export using `"`resultsdir'/survey_logit.csv"'
    quietly margins vs, over(cyl) subpop(domain)
    mfx_export using `"`resultsdir'/survey_logit_by_cyl.csv"'

    postclose mfxpost
    preserve
    use `"`combined'"', clear
    replace fixture = regexs(1) if regexm(fixture, "/([^/]+)[.]csv$")
    export delimited using `"`resultsdir'/misc.csv"', replace
    restore

    display as result "marginaleffects fixtures written to `resultsdir'"
end


capture program drop mfx_runall
program define mfx_runall
    version 17.0
    args model byvar facvar numvar resultsdir

    quietly margins
    mfx_export using `"`resultsdir'/`model'_avg_predictions.csv"'

    quietly margins, over(`byvar')
    mfx_export using `"`resultsdir'/`model'_avg_predictions_by.csv"'

    quietly margins, dydx(i.`facvar')
    mfx_export using `"`resultsdir'/`model'_avg_comparisons_factor.csv"'

    quietly margins, dydx(i.`facvar') over(`byvar')
    mfx_export using `"`resultsdir'/`model'_avg_comparisons_factor_by.csv"'

    quietly margins, dydx(`numvar')
    mfx_export using `"`resultsdir'/`model'_avg_slopes.csv"'

    quietly margins, dydx(`numvar') over(`byvar')
    mfx_export using `"`resultsdir'/`model'_avg_slopes_by.csv"'
end


capture program drop mfx_export
program define mfx_export
    version 17.0
    syntax using/

    tempname table
    matrix `table' = r(table)
    local columns : colfullnames `table'
    local n = colsof(`table')

    forvalues i = 1/`n' {
        local label : word `i' of `columns'
        local estimate = el(`table', 1, `i')
        local std_error = el(`table', 2, `i')
        if (`std_error' != . & `std_error' != 0) {
            post mfxpost (`"`using'"') (`"`label'"') ///
                (`estimate') (`std_error')
        }
    }

    preserve
    clear
    quietly set obs `n'
    generate str244 contrast = ""
    generate double estimate = .
    generate double std_error = .

    forvalues i = 1/`n' {
        local label : word `i' of `columns'
        quietly replace contrast = `"`label'"' in `i'
        quietly replace estimate = el(`table', 1, `i') in `i'
        quietly replace std_error = el(`table', 2, `i') in `i'
    }

    * Base and omitted factor levels are exact-zero placeholders in Stata and
    * have no counterpart in marginaleffects output.
    drop if missing(std_error) | std_error == 0

    export delimited using `"`using'"', replace
    restore
end
