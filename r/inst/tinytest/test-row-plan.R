source("helpers.R")
using("marginaleffects")

sentinel_draws <- function(n, draws = 3) {
    outer(seq_len(n) * 1000, seq_len(draws), "+")
}


# hypothesis formula plans must derive output rows and posterior draw rows from
# the same source positions, even when groups are non-contiguous and labels are
# duplicated.
x <- data.table::data.table(
    estimate = c(10, 20, 35, 40),
    term = c("same", "same", "same", "same"),
    grp = c("b", "a", "b", "a")
)
draws <- sentinel_draws(nrow(x))
attr(x, "posterior_draws") <- draws

h <- marginaleffects:::get_hypothesis(
    x,
    hypothesis = ~ reference | grp,
    newdata = x
)
hd <- attr(h, "posterior_draws")
expect_equivalent(h$grp, c("a", "b"))
expect_equivalent(h$estimate, c(20, 25))
expect_equivalent(hd[1, ], draws[4, ] - draws[2, ])
expect_equivalent(hd[2, ], draws[3, ] - draws[1, ])


# pairwise order follows the old lower-triangle, column-major order:
# row 2 - row 1, row 3 - row 1, row 3 - row 2.
x <- data.table::data.table(
    estimate = c(10, 20, 30),
    term = c("a", "b", "c")
)
draws <- sentinel_draws(nrow(x))
attr(x, "posterior_draws") <- draws

h <- marginaleffects:::get_hypothesis(x, hypothesis = ~ pairwise, newdata = x)
hd <- attr(h, "posterior_draws")
expect_equivalent(h$estimate, c(10, 20, 10))
expect_equivalent(hd[1, ], draws[2, ] - draws[1, ])
expect_equivalent(hd[2, ], draws[3, ] - draws[1, ])
expect_equivalent(hd[3, ], draws[3, ] - draws[2, ])


# by aggregation uses the same positional plan for frequentist estimates and
# Bayesian draw rows.
x <- data.table::data.table(
    estimate = c(10, 20, 30, 40),
    term = "same",
    grp = c("b", "a", "b", "a"),
    marginaleffects_wts_internal = c(1, 2, 3, 4)
)
draws <- sentinel_draws(nrow(x))

b <- marginaleffects:::get_by(
    estimates = data.table::copy(x),
    draws = draws,
    newdata = x,
    by = "grp"
)
bd <- attr(b, "posterior_draws")
expect_equivalent(b$grp, c("a", "b"))
expect_equivalent(
    bd[1, ],
    apply(
        draws[c(2, 4), ],
        2,
        stats::weighted.mean,
        w = x$marginaleffects_wts_internal[c(2, 4)]
    )
)
expect_equivalent(
    bd[2, ],
    apply(
        draws[c(1, 3), ],
        2,
        stats::weighted.mean,
        w = x$marginaleffects_wts_internal[c(1, 3)]
    )
)
expect_equivalent(b$estimate, apply(bd, 1, stats::median))

b_freq <- marginaleffects:::get_by(
    estimates = data.table::copy(x),
    draws = NULL,
    newdata = x,
    by = "grp"
)
expect_equivalent(b_freq$grp, b$grp)
expect_equivalent(b_freq$estimate, c(
    stats::weighted.mean(x$estimate[c(2, 4)], x$marginaleffects_wts_internal[c(2, 4)]),
    stats::weighted.mean(x$estimate[c(1, 3)], x$marginaleffects_wts_internal[c(1, 3)])
))
