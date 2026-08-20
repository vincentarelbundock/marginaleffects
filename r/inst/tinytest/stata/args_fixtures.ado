*! marginaleffects cross-software fixtures for the *argument* surface.
*! Run from Stata with: args_fixtures, resultsdir("results")
*!
*! misc.ado covers a broad model x command matrix at a single setting of
*! everything else: default OIM variance, response scale, no weights, no
*! hypothesis, no at() grid, one over() variable. This file holds the
*! complementary axis -- the same handful of models exercised across
*! marginaleffects' argument surface.
*!
*! Every dataset is pulled from the Rdatasets archive so the R side fits the
*! identical rows, and every derived variable is a deterministic function of
*! the raw columns.

capture program drop args_fixtures
program define args_fixtures
    version 17.0
    syntax [, RESULTSDIR(string)]

    if `"`resultsdir'"' == "" local resultsdir "results"
    capture mkdir `"`resultsdir'"'

    tempfile combined
    mfx_open `"`combined'"'

    local rd "https://vincentarelbundock.github.io/Rdatasets/csv"

    * ======================================================================
    * 1. hypothesis = : Stata contrast operators and pwcompare
    * ======================================================================
    * Stata's contrast operators on `margins` are the direct counterparts of
    * marginaleffects' hypothesis formulas. cyl has three levels, which is the
    * minimum needed for reference/sequential/meandev to differ from each
    * other. Each contrast also carries a "Joint" Wald row, exported as
    * <fixture>_joint, which is hypotheses(..., joint = TRUE).
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)

    quietly logit am c.hp c.wt i.cyl
    mfx_hypotheses "logit" cyl

    quietly regress mpg c.hp c.wt i.cyl
    mfx_hypotheses "lm" cyl

    quietly poisson carb c.hp c.wt i.cyl
    mfx_hypotheses "poisson" cyl

    * Contrasts within groups: hypothesis applied inside each by() level.
    quietly logit am c.hp c.wt i.cyl
    quietly margins r.cyl, over(vs) level(95)
    mfx_contrast, fixture("logit_hyp_reference_by")

    * Contrasts of slopes rather than of predictions.
    quietly margins, dydx(hp) over(cyl) post
    quietly lincom _b[6.cyl] - _b[4.cyl]
    mfx_lincom, fixture("logit_hyp_slope_lincom")

    * String hypotheses: a single linear combination and a joint test.
    quietly logit am c.hp c.wt i.cyl
    quietly margins cyl, post
    quietly lincom _b[6.cyl] - _b[4.cyl]
    mfx_lincom, fixture("logit_hyp_lincom")
    quietly test _b[4.cyl] = _b[6.cyl] = _b[8.cyl]
    local s = r(chi2)
    local p = r(p)
    local d = r(df)
    mfx_joint "logit_hyp_equality_joint" `s' `p' `d'

    * ======================================================================
    * 2. vcov = : robust, clustered, and HC variants
    * ======================================================================
    * The point estimates are invariant to the variance estimator, so these
    * fixtures isolate the delta-method standard errors. For `regress` the
    * mapping is exact: vce(robust) is HC1, and vce(hc2)/vce(hc3) are HC2/HC3.
    * Clustering on a model variable such as cyl leaves only three groups, and
    * Stata then refuses the overall margin because the cluster-robust
    * covariance has rank G-1 < k. clust is deterministic and outside the
    * model, so the R side can rebuild it and vcov = ~clust is well defined.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    generate byte clust = mod(_n - 1, 8) + 1

    quietly regress mpg c.hp c.wt i.cyl, vce(robust)
    mfx_runthree "lm_vcov_hc1" cyl hp
    quietly regress mpg c.hp c.wt i.cyl, vce(hc2)
    mfx_runthree "lm_vcov_hc2" cyl hp
    quietly regress mpg c.hp c.wt i.cyl, vce(hc3)
    mfx_runthree "lm_vcov_hc3" cyl hp
    quietly regress mpg c.hp c.wt i.cyl, vce(cluster clust)
    mfx_runthree "lm_vcov_cluster" cyl hp

    quietly logit am c.hp c.wt i.cyl, vce(robust)
    mfx_runthree "logit_vcov_robust" cyl hp
    quietly logit am c.hp c.wt i.cyl, vce(cluster clust)
    mfx_runthree "logit_vcov_cluster" cyl hp

    quietly poisson carb c.hp c.wt i.cyl, vce(robust)
    mfx_runthree "poisson_vcov_robust" cyl hp

    * ======================================================================
    * 3. type = "link" : margins on the linear-predictor scale
    * ======================================================================
    * predict(xb) skips the inverse-link chain rule entirely, so this is a
    * genuinely different Jacobian path from the response-scale fixtures in
    * misc.ado. Model specifications are deliberately identical to misc.ado's
    * so the R side can share its model-fitting code.
    quietly logit vs c.hp c.wt i.am
    mfx_runall "logit_link" cyl am hp xb
    * Non-canonical link. Stata's Newton-Raphson commands report the observed
    * information while R's glm() reports the expected information from IRLS,
    * and the two coincide only for a canonical link. `irls` puts Stata on the
    * expected information so both sides compute the same variance, which lets
    * the R side use its default vcov instead of a hand-built matrix.
    * ltolerance() is tightened because IRLS and Newton-Raphson stop at
    * slightly different points.
    quietly glm vs c.hp c.wt i.am, family(binomial) link(probit) irls ltolerance(1e-14)
    mfx_runall "probit_link" cyl am hp xb
    quietly poisson carb c.hp c.wt i.am
    mfx_runall "poisson_link" cyl am hp xb
    quietly nbreg carb c.hp c.wt i.am
    mfx_runall "negative_binomial_link" cyl am hp xb
    quietly glm mpg c.hp c.wt i.am, family(gamma) link(log) irls ltolerance(1e-14)
    mfx_runall "gamma_log_link" cyl am hp xb

    * ======================================================================
    * 4. newdata = datagrid(...) : atmeans and at() grids
    * ======================================================================
    * atmeans is only used on an all-continuous model. With factors in the
    * model Stata sets each indicator to its sample mean, producing fractional
    * dummies that datagrid()'s mean_or_mode has no counterpart for; that case
    * is generated below purely to document the difference.
    * A logit whose prediction at the means sits in the extreme tail turns a
    * fourth-decimal optimizer difference into a visible relative difference,
    * because atmeans evaluates a single point rather than averaging over the
    * sample. A linear and a Poisson fit are used instead.
    quietly regress mpg c.hp c.wt
    quietly margins, atmeans level(95)
    mfx_export, fixture("atmeans_lm_predictions")
    quietly margins, dydx(hp wt) atmeans level(95)
    mfx_export, fixture("atmeans_lm_slopes")

    quietly poisson carb c.hp c.wt
    quietly margins, atmeans level(95)
    mfx_export, fixture("atmeans_poisson_predictions")
    quietly margins, dydx(hp wt) atmeans level(95)
    mfx_export, fixture("atmeans_poisson_slopes")

    quietly logit am c.hp c.wt i.cyl
    quietly margins, atmeans level(95)
    mfx_export, fixture("atmeans_predictions_factor")

    * at() holds one variable at fixed values and averages over the observed
    * rows for everything else: a counterfactual datagrid followed by by=.
    quietly logit am c.hp c.wt
    quietly margins, at(hp = (100 150 200)) level(95)
    mfx_export, fixture("at_predictions_hp")
    quietly margins, dydx(hp) at(wt = (2 3 4)) level(95)
    mfx_export, fixture("at_slopes_wt")
    quietly margins, at(hp = (100 200) wt = (2 4)) level(95)
    mfx_export, fixture("at_predictions_grid")

    quietly logit am c.hp c.wt i.cyl
    quietly margins, at(cyl = (4 6 8)) level(95)
    mfx_export, fixture("at_predictions_cyl")

    * ======================================================================
    * 5. wts = : estimation and averaging weights
    * ======================================================================
    * Weights are a deterministic function of an existing column so the R side
    * can rebuild them exactly. Stata reuses the estimation weights when it
    * averages the margins, which is what wts= does in marginaleffects.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    generate double w = mpg
    generate int fw = ceil(wt)

    quietly regress mpg c.hp c.wt i.cyl [aw=w]
    mfx_runthree "wts_lm_aweight" cyl hp
    quietly logit am c.hp c.wt i.cyl [fw=fw]
    mfx_runthree "wts_logit_fweight" cyl hp
    quietly poisson carb c.hp c.wt i.cyl [pw=w], vce(robust)
    mfx_runthree "wts_poisson_pweight" cyl hp

    * ======================================================================
    * 6. comparison = : the averaged nonlinear transforms
    * ======================================================================
    * `margins, post` followed by nlcom is a ratio *of averages*, which is what
    * the "...avg" comparison shortcuts compute. The non-averaged shortcuts
    * average a per-row ratio instead; Stata has no single command for that, so
    * only the averaged family is pinned here.
    quietly logit am c.hp c.wt i.vs
    quietly margins vs, post
    quietly nlcom ///
        (differenceavg: _b[1.vs] - _b[0.vs]) ///
        (ratioavg: _b[1.vs] / _b[0.vs]) ///
        (lnratioavg: ln(_b[1.vs] / _b[0.vs])) ///
        (liftavg: (_b[1.vs] - _b[0.vs]) / _b[0.vs]) ///
        (lnoravg: ln((_b[1.vs] / (1 - _b[1.vs])) / (_b[0.vs] / (1 - _b[0.vs]))))
    mfx_export, fixture("comparison_avg_family")

    * transform = exp applied on top of lnoravg is the odds ratio.
    quietly logit am c.hp c.wt i.vs
    quietly margins vs, post
    quietly nlcom ///
        (oratio: (_b[1.vs] / (1 - _b[1.vs])) / (_b[0.vs] / (1 - _b[0.vs])))
    mfx_export, fixture("comparison_transform_exp")

    * Elasticities recomputed here so the "...avg" slope variants, which take a
    * different Jacobian path to the same quantity, have a reference value.
    import delimited `"`rd'/datasets/mtcars.csv"', clear varnames(1)
    quietly poisson carb c.hp c.wt
    quietly margins, eyex(hp wt) level(95)
    mfx_export, fixture("elast_eyex")
    quietly margins, eydx(hp wt) level(95)
    mfx_export, fixture("elast_eydx")
    quietly margins, dyex(hp wt) level(95)
    mfx_export, fixture("elast_dyex")
    quietly margins, dydx(hp wt) level(95)
    mfx_export, fixture("elast_dydx")

    * ======================================================================
    * 7. by = : more than one grouping variable
    * ======================================================================
    * Stata drops empty cells, so the R side must expect the observed crossing
    * rather than the full Cartesian product.
    quietly poisson carb c.hp c.wt i.cyl
    quietly margins, over(vs am) level(95)
    mfx_export, fixture("by2_predictions")
    quietly margins, dydx(hp) over(vs am) level(95)
    mfx_export, fixture("by2_slopes")
    quietly margins, dydx(i.cyl) over(vs) level(95)
    mfx_export, fixture("by2_comparisons")

    * ======================================================================
    * 8. conf_level = and df = : interval construction
    * ======================================================================
    * `regress` margins use a t reference distribution with e(df_r) degrees of
    * freedom; the MLE commands use z. These fixtures are the only ones that
    * make the exported conf_low/conf_high columns load-bearing.
    quietly regress mpg c.hp c.wt i.cyl
    quietly margins, level(90)
    mfx_export, fixture("level90_lm_predictions")
    quietly margins, dydx(hp) level(90)
    mfx_export, fixture("level90_lm_slopes")
    quietly margins, dydx(hp) level(99)
    mfx_export, fixture("level99_lm_slopes")

    quietly logit am c.hp c.wt i.cyl
    quietly margins, level(90)
    mfx_export, fixture("level90_logit_predictions")
    quietly margins, dydx(hp) level(90)
    mfx_export, fixture("level90_logit_slopes")

    mfx_close `"`combined'"' `"`resultsdir'/args.csv"'

    display as result "marginaleffects argument fixtures written to `resultsdir'"
end

