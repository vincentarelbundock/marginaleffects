*! Post the scalar Wald test left behind by `test` or `testnl`.
*!
*! Capture r(chi2)/r(F), r(df) and r(p) into locals in the caller and hand them
*! over, because `test` results are cleared by the next r-class command. The
*! label follows the same joint_chi2 / joint_F convention as mfx_contrast.
capture program drop mfx_joint
program define mfx_joint
    version 17.0
    args fixture stat pval dfval lab

    if `"`lab'"' == "" local lab "joint_chi2"

    post mfxpost (`"`fixture'"') ("`lab'") (.) (.) ///
        (`stat') (`pval') (.) (.) (`dfval')
end
