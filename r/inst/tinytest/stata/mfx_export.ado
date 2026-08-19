*! Post one Stata results table into the open `mfxpost` postfile.
*! Part of the marginaleffects cross-software test fixtures.
*!
*! The postfile schema is:
*!   fixture contrast estimate std_error statistic p_value conf_low conf_high df
*!
*! Values are posted straight from the matrix rather than through local macros
*! so that full double precision survives into the CSV.
capture program drop mfx_export
program define mfx_export
    version 17.0
    syntax , FIXture(string) [SOURce(string)]

    if `"`source'"' == "" local source "r(table)"

    tempname table b se z p ll ul dfr
    matrix `table' = `source'
    local columns : colfullnames `table'
    local n = colsof(`table')

    forvalues i = 1/`n' {
        local label : word `i' of `columns'
        scalar `se' = el(`table', 2, `i')
        * Base and omitted factor levels are exact-zero placeholders in Stata
        * and have no counterpart in marginaleffects output.
        if (`se' < . & `se' != 0) {
            scalar `b'   = el(`table', 1, `i')
            scalar `z'   = el(`table', 3, `i')
            scalar `p'   = el(`table', 4, `i')
            scalar `ll'  = el(`table', 5, `i')
            scalar `ul'  = el(`table', 6, `i')
            scalar `dfr' = el(`table', 7, `i')
            post mfxpost (`"`fixture'"') (`"`label'"') ///
                (`b') (`se') (`z') (`p') (`ll') (`ul') (`dfr')
        }
    }
end
