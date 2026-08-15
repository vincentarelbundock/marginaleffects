# Benchmark matrix-native aggregation, hypotheses, and predictions against the
# numerical fallback. Run from r/ after loading the development package:
# pkgload::load_all(); source("sandbox/benchmark_jacobian_analytic_extensions.R")

library(marginaleffects)

set.seed(1024)
n <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_N", "10000"))
p <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_P", "50"))
repetitions <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_REPS", "7"))

x <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(x) <- paste0("x", seq_len(p))
dat <- as.data.frame(x)
dat$g <- rep(0:1, length.out = n)
dat$w <- runif(n, 0.5, 2)
dat$y <- drop(x %*% seq_len(p) / p + rnorm(n))
formula <- reformulate(colnames(x), response = "y")
mod <- lm(formula, data = dat)

analytic_function <- get(
    "get_jacobian_analytic",
    envir = asNamespace("marginaleffects")
)
set_fallback <- function(enabled) {
    assignInNamespace(
        "get_jacobian_analytic",
        if (isTRUE(enabled)) function(...) NULL else analytic_function,
        ns = "marginaleffects"
    )
}

cases <- list(
    matrix_hypothesis = function() comparisons(
        mod,
        variables = c("x1", "x2"),
        newdata = "mean",
        hypothesis = matrix(c(1, -1), ncol = 1)
    ),
    weighted_average_hypothesis = function() avg_comparisons(
        mod,
        variables = "x1",
        by = "g",
        wts = "w",
        hypothesis = matrix(c(1, -1), ncol = 1)
    ),
    predictions = function() predictions(mod, newdata = dat),
    weighted_average_predictions = function() avg_predictions(
        mod,
        by = "g",
        wts = "w",
        hypothesis = matrix(c(1, -1), ncol = 1)
    )
)

benchmark_case <- function(fun) {
    invisible(fun())
    modes <- sample(rep(c("analytic", "fallback"), repetitions))
    elapsed <- list(analytic = numeric(), fallback = numeric())
    for (mode in modes) {
        fallback <- identical(mode, "fallback")
        set_fallback(fallback)
        elapsed[[mode]] <- c(
            elapsed[[mode]],
            unname(system.time(invisible(fun()))[["elapsed"]])
        )
    }
    set_fallback(FALSE)
    c(
        analytic = median(elapsed$analytic),
        fallback = median(elapsed$fallback),
        speedup = median(elapsed$fallback) / median(elapsed$analytic),
        reduction = 1 - median(elapsed$analytic) / median(elapsed$fallback)
    )
}

results <- t(vapply(cases, benchmark_case, numeric(4)))
print(results)
