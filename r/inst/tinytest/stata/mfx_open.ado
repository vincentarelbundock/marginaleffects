*! Open the shared fixture postfile. Paired with mfx_close.
capture program drop mfx_open
program define mfx_open
    version 17.0
    args path

    capture postclose mfxpost
    postfile mfxpost str244 fixture str244 contrast ///
        double estimate double std_error double statistic double p_value ///
        double conf_low double conf_high double df ///
        using `"`path'"', replace
end
