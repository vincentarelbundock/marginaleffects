# Benchmark the exact linear-model Jacobian against coefficient perturbation.
# Run from r/: Rscript sandbox/benchmark_jacobian_analytic.R

library(marginaleffects)

set.seed(1024)
n <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_N", "50000"))
p <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_P", "20"))
x <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(x) <- paste0("x", seq_len(p))
dat <- as.data.frame(x)
dat$y <- drop(x %*% seq_len(p) / p + rnorm(n))
mod <- lm(y ~ ., data = dat)

hi <- lo <- dat
hi$x1 <- hi$x1 + 1
X_hi <- model.matrix(mod, data = hi)
X_lo <- model.matrix(mod, data = lo)
beta <- coef(mod)

finite_difference <- function() {
    fun <- function(b) {
        mod_tmp <- marginaleffects::set_coef(mod, b)
        drop(predict(mod_tmp, newdata = hi) - predict(mod_tmp, newdata = lo))
    }
    marginaleffects:::get_jacobian_fdforward(fun, beta)
}

analytic <- function() {
    X_hi - X_lo
}

J_fd <- finite_difference()
J_analytic <- analytic()
jacobian_error <- max(abs(J_fd - J_analytic))
cat("Maximum finite-difference error:", jacobian_error, "\n")
stopifnot(jacobian_error < 1e-3)

run_comparison <- function() {
    invisible(comparisons(mod, variables = "x1", comparison = "difference"))
}

analytic_function <- get(
    "get_jacobian_analytic",
    envir = asNamespace("marginaleffects")
)
set_fallback <- function(enabled) {
    if (isTRUE(enabled)) {
        assignInNamespace(
            "get_jacobian_analytic",
            function(...) NULL,
            ns = "marginaleffects"
        )
    } else {
        assignInNamespace(
            "get_jacobian_analytic",
            analytic_function,
            ns = "marginaleffects"
        )
    }
}

run_comparison()
modes <- sample(rep(c("analytic", "fallback"), 15L))
elapsed <- list(analytic = numeric(), fallback = numeric())
for (mode in modes) {
    fallback <- identical(mode, "fallback")
    if (fallback) set_fallback(TRUE)
    elapsed[[mode]] <- c(
        elapsed[[mode]],
        unname(system.time(run_comparison())[["elapsed"]])
    )
    if (fallback) set_fallback(FALSE)
}

timings <- rbind(
    analytic = quantile(elapsed$analytic, c(.25, .5, .75)),
    fallback = quantile(elapsed$fallback, c(.25, .5, .75))
)
print(timings)
cat("Median speedup:", median(elapsed$fallback) / median(elapsed$analytic), "\n")
cat("Median time reduction:", 1 - median(elapsed$analytic) / median(elapsed$fallback), "\n")
