*! Run the standard six-command matrix against the model in memory.
*!
*! These are the Stata counterparts of avg_predictions(), avg_comparisons()
*! and avg_slopes(), each overall and by group. `predictopt` is optional; when
*! supplied it is passed through as margins' predict() option, which is how the
*! link-scale (type = "link") fixtures are produced. level(95) is stated
*! explicitly so the exported confidence bounds do not depend on `set level`.
capture program drop mfx_runall
program define mfx_runall
    version 17.0
    args model byvar facvar numvar predictopt

    local opt "level(95)"
    if `"`predictopt'"' != "" local opt `"level(95) predict(`predictopt')"'

    quietly margins, `opt'
    mfx_export, fixture("`model'_avg_predictions")

    quietly margins, over(`byvar') `opt'
    mfx_export, fixture("`model'_avg_predictions_by")

    quietly margins, dydx(i.`facvar') `opt'
    mfx_export, fixture("`model'_avg_comparisons_factor")

    quietly margins, dydx(i.`facvar') over(`byvar') `opt'
    mfx_export, fixture("`model'_avg_comparisons_factor_by")

    quietly margins, dydx(`numvar') `opt'
    mfx_export, fixture("`model'_avg_slopes")

    quietly margins, dydx(`numvar') over(`byvar') `opt'
    mfx_export, fixture("`model'_avg_slopes_by")
end
