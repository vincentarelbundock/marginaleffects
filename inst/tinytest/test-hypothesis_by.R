source("helpers.R")
using("marginaleffects")
library("MASS")
# library("brms")

dat <- transform(mtcars, gear = factor(gear))
mod <- polr(gear ~ mpg + qsec, data = dat, Hess = TRUE)
mod <- lm(hp ~ mpg * qsec * am, data = dat)
# mod <- brm(hp ~ mpg * qsec * am, data = dat, backend = "cmdstanr")

# Q
# pkgload::load_all()

fun_hyp_reference <- function(x, newdata = NULL, hypothesis_by = NULL, draws = NULL) {

    flag_extra_columns <- any(!hypothesis_by %in% colnames(x)) && !is.null(newdata)
    if (flag_extra_columns) {
        x <- merge(x, newdata)
        data.table::setDT(x)
    }

    x[, term := get_hypothesis_row_labels(x, newdata = newdata, by = NULL, hypothesis_by = hypothesis_by)]

    if (is.null(hypothesis_by)) {
        out <- x[, list(term = sprintf("%s vs. %s", term, term[1]),
                        estimate = estimate - estimate[1])]
    } else {
        out <- x[, list(term = sprintf("%s vs. %s", term, term[1]),
                        estimate = estimate - estimate[1]), 
                 by = hypothesis_by]
    }

    idx <- out$estimate != 0
    out <- out[idx]

    if (is.matrix(draws)) {
        if (!is.null(hypothesis_by)) {
            g <- data.table(newdata)
            g <- as.list(g[, ..hypothesis_by])
            sta <- collapse::BY(draws, g = g, FUN = function(x) x[1])
            dr <- collapse::TRA(draws, g = g, FUN = "-", STATS = sta)
        } else {
            sta <- collapse::BY(draws, FUN = function(x) x[1])
            dr <- collapse::TRA(draws, FUN = "-", STATS = sta)
        }
        dr <- dr[idx,]
        attr(out, "posterior_draws") <- dr
    }

    return(out)
}



predictions(mod,
    hypothesis_by = "mpg",
    hypothesis = fun_hyp_reference,
    newdata = datagrid(mpg = range, qsec = fivenum)
)



# predictions(mod,
#     hypothesis_by = "mpg",
#     hypothesis = fun_hyp_reference,
#     newdata = datagrid(mpg = range, qsec = fivenum)
# )
