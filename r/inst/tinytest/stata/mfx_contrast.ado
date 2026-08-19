*! Post a `margins` contrast table plus its joint Wald test.
*!
*! Contrast operators (r. a. ar. g. h. p.) and `pwcompare` leave both r(table)
*! and the per-term r(chi2)/r(df)/r(p) matrices behind. The final column of
*! those matrices is Stata's "Joint" row, which is the analogue of
*! marginaleffects' hypotheses(..., joint = TRUE).
*!
*! Likelihood-based commands report a chi-square and `regress` reports an F.
*! The joint row is labelled joint_chi2 or joint_F accordingly, because the two
*! are not comparable to the same R quantity: marginaleffects always reports F,
*! and F x numerator-df is the chi-square.
*!
*! Detecting which one is present takes care: `matrix A = r(chi2)` does not
*! fail when r(chi2) is absent -- Stata falls back to reading it as a scalar
*! and quietly builds a 1x1 missing matrix. The shape is therefore checked
*! against r(df), which is always present with one column per term plus the
*! joint column.
capture program drop mfx_contrast
program define mfx_contrast
    version 17.0
    syntax , FIXture(string) [SOURce(string) NOJoint]

    tempname T C D P
    if `"`source'"' == "" local source "r(table)"
    matrix `T' = `source'

    local lab
    if "`nojoint'" == "" {
        matrix `D' = r(df)
        matrix `P' = r(p)
        local k = colsof(`D')
        local lab "joint_F"
        capture matrix `C' = r(chi2)
        if _rc == 0 {
            if colsof(`C') == `k' {
                if el(`C', 1, `k') < . local lab "joint_chi2"
            }
        }
        if "`lab'" == "joint_F" {
            matrix `C' = r(F)
        }
    }

    mfx_export, fixture(`"`fixture'"') source(`T')

    if "`nojoint'" == "" {
        post mfxpost (`"`fixture'_joint"') ("`lab'") (.) (.) ///
            (el(`C', 1, `k')) (el(`P', 1, `k')) (.) (.) (el(`D', 1, `k'))
    }
end
