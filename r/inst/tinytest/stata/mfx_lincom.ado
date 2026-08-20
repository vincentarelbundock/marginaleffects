*! Post the scalars left behind by `lincom` or `nlcom` on a single expression.
*!
*! Unlike `margins`, `lincom` does not leave an r(table) matrix behind, only
*! r(estimate)/r(se)/r(z)-or-r(t)/r(p)/r(lb)/r(ub). Everything is copied into
*! scalars on the first line so no later command can clear it.
capture program drop mfx_lincom
program define mfx_lincom
    version 17.0
    tempname b se stat p lo hi dfr
    scalar `b'  = r(estimate)
    scalar `se' = r(se)
    scalar `p'  = r(p)
    scalar `lo' = r(lb)
    scalar `hi' = r(ub)
    * MLE commands report z, `regress` and friends report t with r(df).
    if "`r(z)'" != "" {
        scalar `stat' = r(z)
        scalar `dfr'  = .
    }
    else {
        scalar `stat' = r(t)
        scalar `dfr'  = r(df)
    }

    syntax , FIXture(string)

    post mfxpost (`"`fixture'"') ("lincom") ///
        (`b') (`se') (`stat') (`p') (`lo') (`hi') (`dfr')
end
