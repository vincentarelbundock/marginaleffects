*! marginaleffects cross-software fixtures for estimators not covered by
*! misc.ado or by the legacy stata.rds fixtures.
*! Run from Stata with: models_fixtures, resultsdir("results")
*!
*! Where a model supports it, the same six-command matrix as misc.ado is used
*! so results are directly comparable; otherwise the three no-by commands are.
*! Commands whose availability varies by Stata flavour are wrapped in
*! `capture noisily` so one missing estimator cannot abort the whole file --
*! check the log for gaps.

capture program drop models_fixtures
program define models_fixtures
    version 17.0
    syntax [, RESULTSDIR(string)]

    if `"`resultsdir'"' == "" local resultsdir "results"
    capture mkdir `"`resultsdir'"'

    tempfile combined
    mfx_open `"`combined'"'

    local rd "https://vincentarelbundock.github.io/Rdatasets/csv"

    * ======================================================================
    * 1. Plain logit, and the family x link cross-product
    * ======================================================================
    * misc.ado covers probit and cloglog on this specification but never
    * logit, which is the single most used model in the package. The glm rows
    * exercise non-canonical links, where the inverse-link derivative that
    * enters the delta method is not the variance function.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)

    quietly logit vs c.hp c.wt i.am
    mfx_runall "logit" cyl am hp

    quietly glm mpg c.hp c.wt i.am, family(gaussian) link(log) irls ltolerance(1e-14)
    mfx_runall "gaussian_log" cyl am hp

    capture noisily {
        quietly glm carb c.hp c.wt i.am, family(poisson) link(identity) irls ltolerance(1e-14)
        mfx_runall "poisson_identity" cyl am hp
    }

    quietly glm mpg c.hp c.wt i.am, family(igaussian) link(log) irls ltolerance(1e-14)
    mfx_runall "inverse_gaussian_log" cyl am hp

    quietly glm mpg c.hp c.wt i.am, family(gamma) link(power -1)
    mfx_runall "gamma_inverse" cyl am hp

    * Log-binomial (relative risk) regression is deliberately absent: on these
    * data Stata reports e(converged) = 0 and still posts estimates, and a
    * non-converged fit is not a reference value.

    * ======================================================================
    * 2. Offsets / exposure
    * ======================================================================
    * exposure(pop) is offset(log(pop)) in R. The offset must survive into the
    * counterfactual predictions that comparisons and slopes are built from.
    generate double pop = 100 + _n
    quietly poisson carb c.hp c.wt i.am, exposure(pop)
    mfx_runall "poisson_exposure" cyl am hp

    quietly glm carb c.hp c.wt i.am, family(poisson) link(log) exposure(pop)
    mfx_runall "glm_poisson_exposure" cyl am hp

    * ======================================================================
    * 3. Quantile regression away from the median
    * ======================================================================
    quietly qreg mpg c.hp c.wt i.am, quantile(.25)
    mfx_runall "quantile_25" cyl am hp
    quietly qreg mpg c.hp c.wt i.am, quantile(.75)
    mfx_runall "quantile_75" cyl am hp

    * ======================================================================
    * 4. Generalised linear mixed models
    * ======================================================================
    * intmethod(laplace) matches lme4's default nAGQ = 1, and
    * predict(mu fixedonly) matches re.form = NA. Without both, the two
    * packages are estimating and predicting different things.
    * mtcars is far too small for a random-intercept GLMM to converge, so
    * these use the standard teaching datasets instead: Contraception for the
    * binary case and epil for the count case.
    preserve
    import delimited `"`rd'/mlmRev/Contraception.csv"', clear varnames(1)
    generate byte usec = (use == "Y")
    generate byte urbanc = (urban == "Y")
    quietly melogit usec c.age i.urbanc || district:, intmethod(laplace)
    mfx_runthree "mixed_logit" urbanc age "mu fixedonly"

    import delimited `"`rd'/MASS/epil.csv"', clear varnames(1)
    encode trt, generate(trt_f)
    quietly mepoisson y c.lbase i.trt_f c.lage || subject:, intmethod(laplace)
    mfx_runthree "mixed_poisson" trt_f lbase "mu fixedonly"
    restore

    * ======================================================================
    * 5. Nonlinear least squares
    * ======================================================================
    * A genuinely nonlinear mean function, so the Jacobian cannot come from a
    * model matrix. Starting values are stated so R reaches the same optimum.
    capture noisily {
        quietly nl (mpg = {b0=50}*exp({b1=-0.2}*wt)), variables(wt)
        quietly margins, dydx(wt) level(95)
        mfx_export, fixture("nls_slopes")
        quietly margins, level(95)
        mfx_export, fixture("nls_predictions")
    }

    * ======================================================================
    * 6. Interval-censored regression
    * ======================================================================
    * intreg with a finite lower and upper bound is survreg() with
    * Surv(lo, hi, type = "interval2").
    generate double lo = mpg - 1
    generate double hi = mpg + 1
    capture noisily {
        quietly intreg lo hi c.hp c.wt i.am
        mfx_runthree "intreg" am hp
    }

    * ======================================================================
    * 7. Zero-inflated Poisson and left-truncated regression
    * ======================================================================
    import delimited `"`rd'/pscl/bioChemists.csv"', clear varnames(1)
    encode fem, generate(fem_f)
    encode mar, generate(mar_f)

    quietly zip art c.phd c.ment i.fem_f i.mar_f kid5, inflate(c.ment kid5 i.fem_f)
    quietly margins, dydx(phd ment fem_f mar_f kid5) predict(n) level(95)
    mfx_export, fixture("zip_response")
    quietly margins, dydx(ment fem_f kid5) predict(pr) level(95)
    mfx_export, fixture("zip_zero_probability")
    quietly margins, predict(n) level(95)
    mfx_export, fixture("zip_predictions")

    * Left-truncated linear regression: truncreg::truncreg in R.
    preserve
    keep if art > 0
    quietly truncreg art c.phd c.ment i.fem_f, ll(0)
    mfx_runthree "truncreg" fem_f phd
    restore

    * ======================================================================
    * 8. Parametric survival: distributions other than Weibull
    * ======================================================================
    * The `time` option puts streg in the accelerated-failure-time metric,
    * which is the metric survreg() uses. predict(xb) is then type = "lp".
    import delimited `"`rd'/KMsurv/kidtran.csv"', clear varnames(1)
    stset time, failure(delta)

    foreach dist in exponential weibull lognormal loglogistic {
        capture noisily {
            quietly streg c.age i.gender i.race, distribution(`dist') time
            quietly margins, dydx(age gender race) predict(xb) level(95)
            mfx_export, fixture("streg_`dist'_lp_slopes")
            quietly margins, predict(xb) level(95)
            mfx_export, fixture("streg_`dist'_lp_predictions")
        }
    }

    * ======================================================================
    * 9. Ordered probit
    * ======================================================================
    * misc.ado covers ologit on this specification; polr(method = "probit")
    * is the counterpart of oprobit.
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

    quietly oprobit sat_f i.infl_f##i.type_f i.cont_f
    forvalues outcome = 1/3 {
        quietly margins, predict(outcome(`outcome')) level(95)
        mfx_export, fixture("oprobit_predictions_outcome`outcome'")
        quietly margins, dydx(infl_f type_f cont_f) predict(outcome(`outcome')) level(95)
        mfx_export, fixture("oprobit_comparisons_outcome`outcome'")
    }

    * ======================================================================
    * 10. Beta regression
    * ======================================================================
    * Not available in every Stata flavour; guarded accordingly.
    import delimited `"`rd'/betareg/GasolineYield.csv"', clear varnames(1)
    capture noisily {
        quietly betareg yield i.batch temp
        mfx_runthree "betareg" batch temp
    }

    mfx_close `"`combined'"' `"`resultsdir'/models.csv"'

    display as result "marginaleffects model fixtures written to `resultsdir'"
end
