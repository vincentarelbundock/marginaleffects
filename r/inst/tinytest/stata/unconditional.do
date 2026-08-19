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
*   - Stata's ML commands report the OBSERVED information matrix, while
*     sandwich::bread() (which marginaleffects relies on) reports the
*     EXPECTED/Fisher information. The two coincide only for CANONICAL links,
*     so a non-canonical fit estimated by Newton-Raphson disagrees with R by
*     3-10% -- not a disagreement about the estimand, just a different
*     information matrix.
*
*     Rather than bend the R side to Stata's convention, the non-canonical
*     models below are estimated with `glm ..., irls`, which switches Stata to
*     iteratively reweighted least squares and therefore to the expected
*     information -- the same matrix R uses. Both sides then compute the same
*     estimator and agree to ~1e-5, so these fixtures certify agreement instead
*     of merely guarding against regression. IRLS and Newton-Raphson maximise
*     the same likelihood but stop at different points under Stata's default
*     criteria, which moved the estimates by ~3e-4, so the two IRLS fits also
*     tighten `ltolerance()`, which is the criterion IRLS uses.
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

* The shared mfx_* helpers in this directory do the exporting, so these
* fixtures use the same schema and the same base-level filtering as every
* other Stata suite here.


* --- helper: the five estimands, for whichever model is in memory -------------
* `margins` operates on the currently active estimates.
capture program drop runall
program define runall
    args model byvar facvar numvar

    quietly margins, vce(unconditional) level(95)
    mfx_export, fixture("`model'_avg_predictions")

    quietly margins, over(`byvar') vce(unconditional) level(95)
    mfx_export, fixture("`model'_avg_predictions_by")

    quietly margins, dydx(i.`facvar') vce(unconditional) level(95)
    mfx_export, fixture("`model'_avg_comparisons_fac")

    quietly margins, dydx(i.`facvar') over(`byvar') vce(unconditional) level(95)
    mfx_export, fixture("`model'_avg_comparisons_fac_by")

    * On a continuous variable `dydx()` is a derivative, so the marginaleffects
    * counterpart is avg_slopes(), not avg_comparisons(). (On a factor the two
    * coincide -- `dydx(i.fac)` above is the discrete difference.)
    quietly margins, dydx(`numvar') vce(unconditional) level(95)
    mfx_export, fixture("`model'_avg_slopes_num")

    quietly margins, dydx(`numvar') over(`byvar') vce(unconditional) level(95)
    mfx_export, fixture("`model'_avg_slopes_num_by")
end

* --- output file -------------------------------------------------------------
capture mkdir results
tempfile combined
mfx_open `"`combined'"'

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

* binomial / probit -- NON-canonical, so `irls` is used to put Stata on the
* expected information. `glm ... link(probit)` is the same likelihood `probit`
* maximises; only the information matrix and hence the standard errors change.
quietly glm wide slength pwidth i.heavy, family(binomial) link(probit) irls vce(robust) ltolerance(1e-14)
runall "iris_glm_binomial_probit" species heavy slength

* Gamma / log -- NON-canonical, `irls` for the same reason as probit above.
quietly glm swidth slength pwidth i.heavy, family(gamma) link(log) irls vce(robust) ltolerance(1e-14)
runall "iris_glm_gamma_log" species heavy slength

mfx_close `"`combined'"' "results/unconditional.csv"

display "wrote results/unconditional.csv"
exit, clear
