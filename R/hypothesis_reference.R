hypothesis_functions <- list(
    reference = list(
        ratio = list(
            comparison = function(x) x / x[1],
            label = function(x) sprintf("%s / %s", x, x[1]),
            index = function(x) c(FALSE, rep(TRUE, length(x) - 1)),
            by = identity
        ),
        difference = list(
            comparison = function(x) x - x[1],
            label = function(x) sprintf("%s - %s", x, x[1]),
            index = function(x) c(FALSE, rep(TRUE, length(x) - 1)),
            by = identity
        )
    ),
    revreference = list(
        ratio = list(
            comparison = function(x) x[1] / x,
            label = function(x) sprintf("%s / %s", x[1], x),
            index = function(x) c(FALSE, rep(TRUE, length(x) - 1)),
            by = identity
        ),
        difference = list(
            comparison = function(x) x - x[1],
            label = function(x) sprintf("%s - %s", x[1], x),
            index = function(x) c(FALSE, rep(TRUE, length(x) - 1)),
            by = identity
        )
    )
)
