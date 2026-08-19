*! The six contrast operators plus pwcompare, for one factor in one model.
capture program drop mfx_hypotheses
program define mfx_hypotheses
    version 17.0
    args model facvar

    * Baseline margins; the R side builds every hypothesis from these.
    quietly margins `facvar', level(95)
    mfx_export, fixture("`model'_margins_`facvar'")

    * r. each level against the base level          -> hypothesis = ~reference
    quietly margins r.`facvar', level(95)
    mfx_contrast, fixture("`model'_hyp_reference")

    * ar. each level against the previous level     -> hypothesis = ~sequential
    quietly margins ar.`facvar', level(95)
    mfx_contrast, fixture("`model'_hyp_sequential")

    * a. each level against the next level          -> ~revsequential
    quietly margins a.`facvar', level(95)
    mfx_contrast, fixture("`model'_hyp_revsequential")

    * g. each level against the grand mean          -> hypothesis = ~meandev
    quietly margins g.`facvar', level(95)
    mfx_contrast, fixture("`model'_hyp_meandev")

    * h. Helmert                                    -> hypothesis = ~helmert
    quietly margins h.`facvar', level(95)
    mfx_contrast, fixture("`model'_hyp_helmert")

    * p. orthogonal polynomial                      -> hypothesis = ~poly
    quietly margins p.`facvar', level(95)
    mfx_contrast, fixture("`model'_hyp_poly")

    * pwcompare all pairs                           -> hypothesis = ~pairwise
    * The pairwise contrasts live in r(table_vs); r(table) holds the plain
    * margins, and there is no joint row.
    quietly margins `facvar', pwcompare level(95)
    mfx_contrast, fixture("`model'_hyp_pairwise") source(r(table_vs)) nojoint
end
