*! marginaleffects cross-software fixtures from the Rdatasets archive
*! Run from Stata with: misc, resultsdir("results/misc")

capture program drop misc
program define misc
    version 17.0
    syntax [, RESULTSDIR(string)]

    if `"`resultsdir'"' == "" local resultsdir "results/misc"
    capture mkdir `"`resultsdir'"'

    tempfile combined
    mfx_open `"`combined'"'

    local rd "https://vincentarelbundock.github.io/Rdatasets/csv"

    * Heckman selection: selection probability and conditional/unconditional outcome.
    import delimited `"`rd'/sampleSelection/Mroz87.csv"', clear varnames(1)
    quietly heckman wage c.educ c.exper, ///
        select(lfp = c.age c.educ kids5 kids618 c.nwifeinc)
    quietly margins, dydx(age educ kids5 kids618 nwifeinc) predict(psel)
    mfx_export, fixture("heckman_selection")
    quietly margins, dydx(educ exper) predict(ycond)
    mfx_export, fixture("heckman_outcome_conditional")
    quietly margins, dydx(educ exper) predict(yexpected)
    mfx_export, fixture("heckman_outcome_unconditional")

    * Zero-inflated negative binomial: overall mean and both equation components.
    import delimited `"`rd'/pscl/bioChemists.csv"', clear varnames(1)
    encode fem, generate(fem_f)
    encode mar, generate(mar_f)
    quietly zinb art c.phd c.ment i.fem_f i.mar_f kid5, inflate(c.ment kid5 i.fem_f)
    quietly margins, dydx(phd ment fem_f mar_f kid5) predict(n)
    mfx_export, fixture("zinb_response")
    quietly margins, dydx(phd ment fem_f mar_f kid5) expression(exp(predict(xb)))
    mfx_export, fixture("zinb_count_mean")
    quietly margins, dydx(ment fem_f kid5) predict(pr)
    mfx_export, fixture("zinb_zero_probability")

    * Positive-count (hurdle count component) model.
    preserve
    keep if art > 0
    quietly tpoisson art c.phd c.ment i.fem_f i.mar_f kid5, ll(0)
    quietly margins, dydx(phd ment fem_f mar_f kid5) predict(n)
    mfx_export, fixture("hurdle_positive_count")
    restore

    * Binary changes, interactions, by-groups, and weights.
    * Use iris rather than mtcars here: 150 rows give interaction and grouped
    * margins enough support for stable estimation.
    import delimited `"`rd'/datasets/iris.csv"', clear varnames(1)
    generate byte wide = sepalwidth > 3
    * The cut point sits between two observed values rather than on one of
    * them. iris is imported as float and Stata promotes the literal to double,
    * so a threshold of 5.8 would put the rows holding exactly 5.8 on the
    * greater-than side and the indicator would not match the one R builds.
    generate byte long_sepal = sepallength > 5.85
    generate int scount = round(2 * sepalwidth)
    encode species, generate(species_f)
    quietly logit wide c.petallength##c.petalwidth i.long_sepal, vce(robust)
    quietly margins, dydx(long_sepal)
    mfx_export, fixture("logit_binary")
    quietly margins long_sepal, over(species_f)
    mfx_export, fixture("logit_binary_by_species")
    quietly margins, dydx(petallength) at(petalwidth = (0.5 1.5 2.5))
    mfx_export, fixture("logit_interaction_at")

    generate double aw = _n
    quietly poisson scount c.sepallength##c.petallength i.long_sepal [pw=aw], vce(robust)
    quietly margins, dydx(long_sepal sepallength)
    mfx_export, fixture("poisson_weighted_robust")

    * Nonlinear transformations and delta-method standard errors.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    quietly logit am c.hp c.wt i.vs
    quietly margins, dydx(hp)
    mfx_export, fixture("logit_dydx")
    quietly margins vs, post
    quietly nlcom ///
        (ratio: _b[1.vs] / _b[0.vs]) ///
        (lnratio: ln(_b[1.vs] / _b[0.vs])) ///
        (lift: (_b[1.vs] - _b[0.vs]) / _b[0.vs]) ///
        (lnor: ln((_b[1.vs] / (1 - _b[1.vs])) / (_b[0.vs] / (1 - _b[0.vs]))))
    mfx_export, fixture("logit_nonlinear")

    quietly poisson carb c.hp c.wt
    quietly margins, eyex(hp wt)
    mfx_export, fixture("poisson_eyex")
    quietly margins, eydx(hp wt)
    mfx_export, fixture("poisson_eydx")
    quietly margins, dyex(hp wt)
    mfx_export, fixture("poisson_dyex")

    * A broad model/command matrix. Each fit is exercised with the Stata
    * counterparts of avg_predictions(), avg_comparisons(), and avg_slopes(),
    * both overall and by group.
    quietly regress mpg c.hp c.wt i.am
    mfx_runall "lm" cyl am hp

    * Non-canonical link. Stata's Newton-Raphson commands report the observed
    * information while R's glm() reports the expected information from IRLS,
    * and the two coincide only for a canonical link. `irls` puts Stata on the
    * expected information so both sides compute the same variance, which lets
    * the R side use its default vcov instead of a hand-built matrix.
    * ltolerance() is tightened because IRLS and Newton-Raphson stop at
    * slightly different points.
    quietly glm vs c.hp c.wt i.am, family(binomial) link(probit) irls ltolerance(1e-14)
    mfx_runall "probit" cyl am hp

    * Non-canonical link. Stata's Newton-Raphson commands report the observed
    * information while R's glm() reports the expected information from IRLS,
    * and the two coincide only for a canonical link. `irls` puts Stata on the
    * expected information so both sides compute the same variance, which lets
    * the R side use its default vcov instead of a hand-built matrix.
    * ltolerance() is tightened because IRLS and Newton-Raphson stop at
    * slightly different points.
    quietly glm vs c.hp c.wt i.am, family(binomial) link(cloglog) irls ltolerance(1e-14)
    mfx_runall "cloglog" cyl am hp

    quietly poisson carb c.hp c.wt i.am
    mfx_runall "poisson" cyl am hp

    quietly nbreg carb c.hp c.wt i.am
    mfx_runall "negative_binomial" cyl am hp

    quietly glm mpg c.hp c.wt i.am, family(gamma) link(log) irls ltolerance(1e-14)
    mfx_runall "gamma_log" cyl am hp

    quietly qreg mpg c.hp c.wt i.am
    mfx_runall "quantile_median" cyl am hp

    generate double mpg_censored = max(mpg, 18)
    quietly tobit mpg_censored c.hp c.wt i.am, ll(18)
    mfx_runall "tobit_left" cyl am hp

    generate double mpg_fraction = mpg / 40
    quietly fracreg logit mpg_fraction c.hp c.wt i.am
    mfx_runall "fractional_logit" cyl am hp

    * Representative-value predictions and interaction contrasts exercise
    * newdata/datagrid paths rather than only averaging over observed rows.
    import delimited `"`rd'/datasets/iris.csv"', clear varnames(1)
    generate byte wide = sepalwidth > 3
    * The cut point sits between two observed values rather than on one of
    * them. iris is imported as float and Stata promotes the literal to double,
    * so a threshold of 5.8 would put the rows holding exactly 5.8 on the
    * greater-than side and the indicator would not match the one R builds.
    generate byte long_sepal = sepallength > 5.85
    quietly logit wide c.petallength##i.long_sepal c.petalwidth
    quietly margins, at(petallength = (1.5 4.5 6.5) long_sepal = (0 1))
    mfx_export, fixture("logit_predictions_at")
    quietly margins, dydx(long_sepal) at(petallength = (1.5 4.5 6.5))
    mfx_export, fixture("logit_comparisons_at")

    * Clustered/correlated models. The grouping variable is deterministic so
    * the same Rdatasets CSV can be reconstructed exactly on the R side.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    generate byte panel = mod(_n - 1, 8) + 1
    quietly mixed mpg c.hp c.wt i.am || panel:, mle
    mfx_runall "mixed_gaussian" cyl am hp

    xtset panel
    quietly xtgee carb c.hp c.wt i.am, family(poisson) link(log) corr(exchangeable)
    mfx_runall "gee_poisson" cyl am hp

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
    mfx_export, fixture("ologit_predictions_outcome1")
    quietly margins, predict(outcome(2))
    mfx_export, fixture("ologit_predictions_outcome2")
    quietly margins, predict(outcome(3))
    mfx_export, fixture("ologit_predictions_outcome3")
    forvalues outcome = 1/3 {
        quietly margins, dydx(infl_f type_f cont_f) predict(outcome(`outcome'))
        mfx_export, fixture("ologit_comparisons_outcome`outcome'")
    }

    * Multinomial probabilities, interactions, and outcome-specific contrasts.
    * Reuse the expanded housing data so the nominal and ordinal fits are
    * directly comparable and avoid quasi-separation in the iris species data.
    quietly mlogit sat_f i.infl_f##i.cont_f i.type_f, baseoutcome(1)
    forvalues outcome = 1/3 {
        quietly margins, predict(outcome(`outcome'))
        mfx_export, fixture("mlogit_predictions_outcome`outcome'")
        quietly margins, dydx(infl_f cont_f type_f) predict(outcome(`outcome'))
        mfx_export, fixture("mlogit_comparisons_outcome`outcome'")
    }

    * Survival models. Cox supports LP/relative-hazard margins; parametric
    * Weibull additionally supports mean and median survival-time margins.
    import delimited `"`rd'/KMsurv/kidtran.csv"', clear varnames(1)
    stset time, failure(delta)
    quietly stcox c.age i.gender i.race
    quietly margins, dydx(age gender race) predict(xb)
    mfx_export, fixture("cox_lp")
    quietly margins, dydx(age gender race) predict(hr)
    mfx_export, fixture("cox_hazard_ratio")
    quietly streg c.age i.gender i.race, distribution(weibull)
    quietly margins, dydx(age gender race) predict(hr)
    mfx_export, fixture("weibull_hazard_ratio")
    quietly margins, at(age = (30 50 70)) predict(mean time)
    mfx_export, fixture("weibull_mean_survival")
    quietly margins, at(age = (30 50 70)) predict(median time)
    mfx_export, fixture("weibull_median_survival")

    * Survey-weighted/domain margins and by-groups.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    generate long psu = _n
    generate byte strata = mod(_n - 1, 4) + 1
    generate double pwt = mpg
    generate byte domain = cyl != 8
    svyset psu [pweight=pwt], strata(strata) singleunit(centered)
    quietly svy: logit am c.hp i.vs c.wt
    quietly margins, dydx(hp vs) subpop(domain)
    mfx_export, fixture("survey_logit")
    quietly margins vs, over(cyl) subpop(domain)
    mfx_export, fixture("survey_logit_by_cyl")

    mfx_close `"`combined'"' `"`resultsdir'/misc.csv"'

    display as result "marginaleffects fixtures written to `resultsdir'"
end


