* ---------------------------------------------------------------------------
* Golden values for `vcov = vcovUnconditional()`.
*
* Produces `results/unconditional.csv`, consumed by
* `inst/tinytest/test-vcov_unconditional-stata.R`.
*
* Run it with the licensed nix build (batch mode leaves cwd elsewhere, hence
* the `cd` below):
*     cd ~/nix-config/recipes/stata-se
*     make batch dofile=~/repos/marginaleffects/r/inst/tinytest/stata/unconditional.do
*
* Stata's `margins, vce(unconditional)` is the analogue of marginaleffects'
* `vcovUnconditional()`. It requires `vce(robust)` on the estimation command.
*
* EXACTNESS (see snhansen, PR #1737):
*   - Stata uses the OBSERVED information matrix; sandwich::bread() (which
*     marginaleffects relies on) uses the EXPECTED/Fisher information. These
*     coincide only for CANONICAL links, so only the canonical-link models
*     below are expected to match to high precision. Non-canonical links
*     agree to ~3-4 significant digits, not exactly.
*   - Stata scales the unconditional variance by n / (n - k), so the R side
*     compares against `vcovUnconditional(type = "HC1")`.
* ---------------------------------------------------------------------------

clear all
set more off

* `import delimited` stores numeric columns as `float` by default, which costs
* ~7 significant digits and perturbs the iris estimates around 1e-8. Numeric
* columns must be doubles to match R.
set type double

cd ~/repos/marginaleffects/r/inst/tinytest/stata

* --- helper: dump r(b) / r(V) from the last `margins` call --------------------
capture program drop mesave
program define mesave
    args model command
    tempname b V
    matrix `b' = r(b)
    matrix `V' = r(V)
    local names : colnames `b'
    local n = colsof(`b')
    local row = 0
    forvalues i = 1/`n' {
        local nm : word `i' of `names'
        local est = `b'[1, `i']
        local se  = sqrt(`V'[`i', `i'])
        * `margins` emits base and omitted levels as exact zeros. They carry no
        * information and have no counterpart in the marginaleffects output.
        if (`se' == 0) continue
        local ++row
        file write golden ///
            "`model'," "`command'," "`row'," "`nm'," ///
            "`=string(`est', "%20.15g")'," "`=string(`se', "%20.15g")'" _n
    }
end


* --- helper: the five estimands, for whichever model is in memory -------------
* `margins` operates on the currently active estimates.
capture program drop runall
program define runall
    args model byvar facvar numvar

    * (1) avg_predictions(m)
    quietly margins, vce(unconditional)
    mesave "`model'" "avg_predictions"

    * (2) avg_predictions(m, by = byvar)
    quietly margins, over(`byvar') vce(unconditional)
    mesave "`model'" "avg_predictions_by"

    * (3) avg_comparisons(m, variables = facvar)  [factor: discrete difference]
    quietly margins, dydx(i.`facvar') vce(unconditional)
    mesave "`model'" "avg_comparisons_fac"

    * (4) avg_comparisons(m, variables = facvar, by = byvar)
    quietly margins, dydx(i.`facvar') over(`byvar') vce(unconditional)
    mesave "`model'" "avg_comparisons_fac_by"

    * (5) avg_slopes(m, variables = numvar)
    * On a continuous variable `dydx()` is a derivative, so the marginaleffects
    * counterpart is avg_slopes(), not avg_comparisons(). (On a factor the two
    * coincide -- `dydx(i.fac)` above is the discrete difference.)
    quietly margins, dydx(`numvar') vce(unconditional)
    mesave "`model'" "avg_slopes_num"

    * (6) avg_slopes(m, variables = numvar, by = byvar)
    quietly margins, dydx(`numvar') over(`byvar') vce(unconditional)
    mesave "`model'" "avg_slopes_num_by"
end

* --- output file -------------------------------------------------------------
capture mkdir results
file open golden using "results/unconditional.csv", write replace
file write golden "model,command,index,term,estimate,std_error" _n

* =============================================================================
* iris (n = 150). Built from R's `datasets::iris` by databases/generate_iris.R.
* The R test reads this same CSV, so both sides cannot drift apart.
*
* Three derived variables stand in for what iris lacks:
*   wide   = Sepal.Width  > 3        binomial outcome
*   heavy  = Petal.Length > 4        factor to contrast
*   scount = round(2 * Sepal.Width)  count outcome, by coarsening
* by = species, numeric contrast = slength
* =============================================================================
import delimited databases/iris.csv, clear varnames(1)

* gaussian / identity -- canonical
quietly regress swidth slength pwidth i.heavy, vce(robust)
runall "iris_lm_gaussian_identity" species heavy slength

* binomial / logit -- canonical
quietly logit wide slength pwidth i.heavy, vce(robust)
runall "iris_glm_binomial_logit" species heavy slength

* poisson / log -- canonical (count from a coarsened measurement)
quietly poisson scount slength pwidth i.heavy, vce(robust)
runall "iris_glm_poisson_log" species heavy slength

* binomial / probit -- NON-canonical
quietly probit wide slength pwidth i.heavy, vce(robust)
runall "iris_glm_binomial_probit" species heavy slength

* Gamma / log -- NON-canonical
quietly glm swidth slength pwidth i.heavy, family(gamma) link(log) vce(robust)
runall "iris_glm_gamma_log" species heavy slength

file close golden

display "wrote results/unconditional.csv"
exit, clear
