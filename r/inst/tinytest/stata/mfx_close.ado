*! Close the shared fixture postfile and write it out as CSV.
capture program drop mfx_close
program define mfx_close
    version 17.0
    args path csv

    postclose mfxpost
    preserve
    use `"`path'"', clear
    export delimited using `"`csv'"', replace
    restore
end
