sanitize_conf_level <- function(conf_level, ...) {
    # periods in arg name are bad style because of s3, but we want to accept both because `broom` uses `conf_level`

    dots <- list(...)

    if ("conf.level" %in% names(dots)) {
        conf_level <- dots[["conf.level"]]
    }

    checkmate::assert(
        checkmate::check_numeric(conf_level, len = 1),
        checkmate::check_true(conf_level > 0),
        checkmate::check_true(conf_level < 1),
        combine = "and")

    return(conf_level)
}
