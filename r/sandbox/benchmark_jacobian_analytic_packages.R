# Benchmark package-specific analytic Jacobians against the public fallback
# option. Run after installing/loading the development version of the package.

library(marginaleffects)

required <- c("quantreg", "ivreg", "geepack", "rms")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0L) {
    stop("Missing benchmark packages: ", paste(missing, collapse = ", "))
}

set.seed(48103)
n <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_N", "10000"))
p <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_P", "15"))
repetitions <- as.integer(Sys.getenv("MARGINALEFFECTS_BENCH_REPS", "5"))

x <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(x) <- paste0("x", seq_len(p))
dat <- as.data.frame(x)
dat$z <- 0.7 * dat$x1 + rnorm(n)
dat$y <- drop(x %*% seq_len(p) / (2 * p) + rnorm(n))
dat$y_bin <- rbinom(n, 1, plogis(-0.3 + 0.2 * dat$x1 - 0.1 * dat$x2))
dat$id <- rep(seq_len(ceiling(n / 10)), each = 10, length.out = n)

xnames <- colnames(x)
linear_formula <- stats::reformulate(xnames, response = "y")
binary_formula <- stats::reformulate(xnames, response = "y_bin")
iv_formula <- stats::as.formula(sprintf(
    "y ~ %s | %s",
    paste(xnames, collapse = " + "),
    paste(c("z", xnames[-1]), collapse = " + ")
))

benchmark_models <- list(
    rq = quantreg::rq(linear_formula, data = dat),
    ivreg = ivreg::ivreg(iv_formula, data = dat),
    geeglm = suppressWarnings(geepack::geeglm(
        binary_formula,
        id = id,
        data = dat,
        family = stats::binomial(),
        corstr = "independence"
    )),
    lrm = rms::lrm(binary_formula, data = dat, x = TRUE, y = TRUE)
)

benchmark_cases <- list(
    rq = function() avg_comparisons(benchmark_models$rq, variables = "x1"),
    ivreg = function() avg_comparisons(benchmark_models$ivreg, variables = "x1"),
    geeglm = function() avg_comparisons(
        benchmark_models$geeglm,
        variables = "x1",
        type = "response"
    ),
    lrm = function() avg_comparisons(
        benchmark_models$lrm,
        variables = "x1",
        type = "fitted"
    )
)

benchmark_case <- function(fun) {
    old_option <- getOption("marginaleffects_analytic_jacobian")
    on.exit(options(marginaleffects_analytic_jacobian = old_option), add = TRUE)

    options(marginaleffects_analytic_jacobian = TRUE)
    analytic_result <- fun()
    options(marginaleffects_analytic_jacobian = FALSE)
    fallback_result <- fun()
    stopifnot(isTRUE(all.equal(
        components(analytic_result, "jacobian"),
        components(fallback_result, "jacobian"),
        tolerance = 1e-5,
        check.attributes = FALSE
    )))

    elapsed <- list(analytic = numeric(), fallback = numeric())
    modes <- sample(rep(c("analytic", "fallback"), repetitions))
    for (mode in modes) {
        options(marginaleffects_analytic_jacobian = identical(mode, "analytic"))
        gc(FALSE)
        elapsed[[mode]] <- c(
            elapsed[[mode]],
            unname(system.time(invisible(fun()))[["elapsed"]])
        )
    }
    c(
        analytic = stats::median(elapsed$analytic),
        fallback = stats::median(elapsed$fallback),
        speedup = stats::median(elapsed$fallback) /
            stats::median(elapsed$analytic)
    )
}

benchmark_results <- t(vapply(benchmark_cases, benchmark_case, numeric(3)))
print(benchmark_results)
