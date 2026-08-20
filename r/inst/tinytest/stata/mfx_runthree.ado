*! predictions / comparisons / slopes for one fitted model, no by-groups.
capture program drop mfx_runthree
program define mfx_runthree
    version 17.0
    args model facvar numvar predictopt

    local opt "level(95)"
    if `"`predictopt'"' != "" local opt `"level(95) predict(`predictopt')"'

    quietly margins, `opt'
    mfx_export, fixture("`model'_predictions")

    quietly margins, dydx(i.`facvar') `opt'
    mfx_export, fixture("`model'_comparisons")

    quietly margins, dydx(`numvar') `opt'
    mfx_export, fixture("`model'_slopes")
end
