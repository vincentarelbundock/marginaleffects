# Profile the exact matrix Jacobian against the numerical fallback.
# Run from r/ after loading the development package:
# pkgload::load_all(); source("sandbox/profile_jacobian_analytic.R")

library(marginaleffects)

set.seed(1024)
n <- as.integer(Sys.getenv("MARGINALEFFECTS_PROFILE_N", "50000"))
p <- as.integer(Sys.getenv("MARGINALEFFECTS_PROFILE_P", "100"))
iterations <- as.integer(Sys.getenv("MARGINALEFFECTS_PROFILE_ITER", "6"))
profile_mode <- Sys.getenv("MARGINALEFFECTS_PROFILE_MODE", "analytic")
profile_output <- Sys.getenv(
    "MARGINALEFFECTS_PROFILE_OUTPUT",
    file.path(tempdir(), paste0("jacobian-", profile_mode, ".rds"))
)
stopifnot(profile_mode %in% c("analytic", "fallback"))

x <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(x) <- paste0("x", seq_len(p))
dat <- as.data.frame(x)
dat$y <- drop(x %*% seq_len(p) / p + rnorm(n))
mod <- lm(y ~ ., data = dat)

run_comparison <- function() {
    invisible(comparisons(
        mod,
        variables = "x1",
        comparison = "difference"
    ))
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

# Select one mode per R process. Keeping the namespace binding fixed avoids
# contaminating profvis samples when the implementation is toggled in-process.
set_fallback(identical(profile_mode, "fallback"))

# Prove which derivative route this process uses. This probe stays outside
# profvis so tracing does not contaminate the profile.
fd_calls <- 0L
jacobian_calls <- 0L
trace(
    "get_jacobian_fdforward",
    where = asNamespace("marginaleffects"),
    tracer = quote(fd_calls <<- fd_calls + 1L),
    print = FALSE
)
trace(
    "get_jacobian",
    where = asNamespace("marginaleffects"),
    tracer = quote(jacobian_calls <<- jacobian_calls + 1L),
    print = FALSE
)
run_comparison()
untrace("get_jacobian_fdforward", where = asNamespace("marginaleffects"))
untrace("get_jacobian", where = asNamespace("marginaleffects"))
cat("Profile mode:", profile_mode, "\n")
cat("Jacobian dispatcher calls:", jacobian_calls, "\n")
cat("Forward-difference calls:", fd_calls, "\n")
if (identical(profile_mode, "analytic")) {
    stopifnot(jacobian_calls == 0L, fd_calls == 0L)
} else {
    stopifnot(jacobian_calls > 0L, fd_calls > 0L)
}

# Warm caches before sampling, then profile several calls to stabilize the
# one-millisecond sampling data used by profvis and debrief.
run_comparison()
profile_elapsed <- system.time(
    profile <- profvis::profvis({
        for (i in seq_len(iterations)) run_comparison()
    }, interval = 0.001)
)[["elapsed"]]
saveRDS(profile, profile_output)
cat("Profiled wall time:", profile_elapsed, "seconds\n")
cat("Profile saved to:", profile_output, "\n")

cat("\n=== debrief ===\n")
debrief::pv_print_debrief(
    profile,
    n_functions = 15,
    n_lines = 15,
    n_paths = 10,
    n_memory = 10
)
