wmean <- function(x, w) {
    stats::weighted.mean(x, w)
}

comparison_function_dict <- list(
    # default = difference between predictions
    "difference" = function(hi, lo) hi - lo,
    "differenceavg" = function(hi, lo) mean(hi - lo),
    "differenceavgwts" = function(hi, lo, w) wmean(hi - lo, w),

    # slopes and elasticities
    "dydx" = function(hi, lo, eps) (hi - lo) / eps,
    "eyex" = function(hi, lo, eps, y, x) (hi - lo) / eps * (x / y),
    "eydx" = function(hi, lo, eps, y, x) ((hi - lo) / eps) / y,
    "dyex" = function(hi, lo, eps, x) ((hi - lo) / eps) * x,

    # average slopes and elasticities
    "dydxavg" = function(hi, lo, eps) mean((hi - lo) / eps),
    "eyexavg" = function(hi, lo, eps, y, x) mean((hi - lo) / eps * (x / y)),
    "eydxavg" = function(hi, lo, eps, y, x) mean(((hi - lo) / eps) / y),
    "dyexavg" = function(hi, lo, eps, x) mean(((hi - lo) / eps) * x),
    "dydxavgwts" = function(hi, lo, eps, w) wmean((hi - lo) / eps, w),
    "eyexavgwts" = function(hi, lo, eps, y, x, w) wmean((hi - lo) / eps * (x / y), w),
    "eydxavgwts" = function(hi, lo, eps, y, x, w) wmean(((hi - lo) / eps) / y, w),
    "dyexavgwts" = function(hi, lo, eps, x, w) wmean(((hi - lo) / eps) * x, w),

    # ratios
    "ratio" = function(hi, lo) hi / lo,
    "ratioavg" = function(hi, lo) mean(hi) / mean(lo),
    "ratioavgwts" = function(hi, lo, w) wmean(hi, w) / wmean(lo, w),

    "lnratio" = function(hi, lo) log(hi / lo),
    "lnratioavg" = function(hi, lo) log(mean(hi) / mean(lo)),
    "lnratioavgwts" = function(hi, lo, w) log(wmean(hi, w) / wmean(lo, w)),

    "lnor" = function(hi, lo) log((hi / (1 - hi)) / (lo / (1 - lo))),
    "lnoravg" = function(hi, lo) log((mean(hi) / (1 - mean(hi))) / (mean(lo) / (1 - mean(lo)))),
    "lnoravgwts" = function(hi, lo, w)
        log(
            (wmean(hi, w) / (1 - wmean(hi, w))) / (wmean(lo, w) / (1 - wmean(lo, w)))
        ),

    # others
    "lift" = function(hi, lo) (hi - lo) / lo,
    "liftavg" = function(hi, lo) (mean(hi - lo)) / mean(lo),
    "liftavgwts" = function(hi, lo, w) (wmean(hi - lo, w)) / wmean(lo, w),

    "expdydx" = function(hi, lo, eps) ((exp(hi) - exp(lo)) / exp(eps)) / eps,
    "expdydxavg" = function(hi, lo, eps) mean(((exp(hi) - exp(lo)) / exp(eps)) / eps),
    "expdydxavgwts" = function(hi, lo, eps, w) wmean(((exp(hi) - exp(lo)) / exp(eps)) / eps, w)
)

# Exact derivatives of the built-in comparison functions.
#
# Every entry mirrors one closed-form definition in
# `comparison_function_dict` directly above, differentiated by hand with
# respect to the `hi` and `lo` prediction vectors. The two tables sit in one
# file so that a shorthand cannot be added or edited here without its
# derivative in view; R/inst/tinytest/test-comparison-registry.R asserts that
# they stay in step. Closed forms carry no step size, no structural
# assumption, and no verification burden: they are correct wherever they are
# finite, and where they are not finite the caller's finiteness check rejects
# the group and the estimand falls back to the numeric path.
#
# Only recorded built-ins are differentiated. A user-supplied comparison
# closure is arbitrary code, and no finite set of probe evaluations can prove
# structural facts about arbitrary code -- a function built to agree with the
# probes and disagree elsewhere defeats any such scheme. Unknown keys
# therefore return NULL and keep the whole estimand on the numeric
# whole-pipeline path.
#
# `eyex` and `eydx` never reach this table because their `y` formal marks
# their groups `uses_y`, which disqualifies the analytic path upstream.
comparison_gradient_exact <- function(fun_key, hi, lo, args) {
  n <- length(hi)

  # Normalized averaging weights: NULL for rowwise keys, else a vector
  # summing to 1 which also encodes plain means.
  avg <- grepl("avg", fun_key, fixed = TRUE)
  a <- NULL
  if (avg) {
    if (grepl("wts$", fun_key)) {
      w <- args[["w"]]
      if (
        !is.numeric(w) || length(w) != n || any(!is.finite(w)) || sum(w) == 0
      ) {
        return(NULL)
      }
      a <- w / sum(w)
    } else {
      a <- rep.int(1 / n, n)
    }
  }
  wmean_or_mean <- function(x) if (is.null(a)) mean(x) else sum(a * x)

  # Slope-family keys divide by the recorded step; validate it once.
  eps <- NULL
  if (fun_key %in% c(
    "dydx", "dydxavg", "dydxavgwts",
    "dyex", "dyexavg", "dyexavgwts",
    "expdydx", "expdydxavg", "expdydxavgwts"
  )) {
    eps <- args[["eps"]]
    if (
      !is.numeric(eps) || !length(eps) %in% c(1L, n) ||
        any(!is.finite(eps)) || any(eps == 0)
    ) {
      return(NULL)
    }
  }
  x <- args[["x"]]

  switch(fun_key,
    difference = list(hi = rep.int(1, n), lo = rep.int(-1, n)),
    differenceavg = ,
    differenceavgwts = list(hi = a, lo = -a),
    ratio = ,
    lift = list(hi = 1 / lo, lo = -hi / lo^2),
    ratioavg = ,
    ratioavgwts = ,
    liftavg = ,
    liftavgwts = {
      # liftavg = wmean(hi - lo) / wmean(lo) = wmean(hi) / wmean(lo) - 1,
      # so its gradient is the gradient of ratioavg.
      mh <- wmean_or_mean(hi)
      ml <- wmean_or_mean(lo)
      list(hi = a / ml, lo = -a * mh / ml^2)
    },
    lnratio = list(hi = 1 / hi, lo = -1 / lo),
    lnratioavg = ,
    lnratioavgwts = {
      mh <- wmean_or_mean(hi)
      ml <- wmean_or_mean(lo)
      list(hi = a / mh, lo = -a / ml)
    },
    lnor = list(
      hi = 1 / (hi * (1 - hi)),
      lo = -1 / (lo * (1 - lo))
    ),
    lnoravg = ,
    lnoravgwts = {
      mh <- wmean_or_mean(hi)
      ml <- wmean_or_mean(lo)
      list(hi = a / (mh * (1 - mh)), lo = -a / (ml * (1 - ml)))
    },
    dydx = list(hi = rep_len(1 / eps, n), lo = rep_len(-1 / eps, n)),
    dydxavg = ,
    dydxavgwts = list(hi = a / eps, lo = -a / eps),
    dyex = {
      if (!is.numeric(x) || !length(x) %in% c(1L, n)) {
        return(NULL)
      }
      list(hi = rep_len(x / eps, n), lo = rep_len(-x / eps, n))
    },
    dyexavg = ,
    dyexavgwts = {
      if (!is.numeric(x) || !length(x) %in% c(1L, n)) {
        return(NULL)
      }
      list(hi = a * x / eps, lo = -a * x / eps)
    },
    expdydx = list(
      hi = exp(hi) / (exp(eps) * eps),
      lo = -exp(lo) / (exp(eps) * eps)
    ),
    expdydxavg = ,
    expdydxavgwts = list(
      hi = a * exp(hi) / (exp(eps) * eps),
      lo = -a * exp(lo) / (exp(eps) * eps)
    ),
    NULL
  )
}


# Comparison keys the derivative table above is not expected to handle: `eyex`
# and `eydx` and their averaged variants divide by the observed outcome `y`,
# which marks their groups `uses_y` and disqualifies the analytic path
# upstream, so no closed form is recorded for them by design. Kept as data so
# the registry test can assert the exclusion rather than hardcode it twice.
comparison_gradient_excluded <- c(
    "eyex", "eydx",
    "eyexavg", "eydxavg",
    "eyexavgwts", "eydxavgwts"
)


comparison_label_dict <- list(
    "difference" = "%s - %s",
    "differenceavg" = "%s - %s",
    "differenceavgwts" = "%s - %s",
    "dydx" = "dY/dX",
    "eyex" = "eY/eX",
    "eydx" = "eY/dX",
    "dyex" = "dY/eX",
    "dydxavg" = "dY/dX",
    "eyexavg" = "eY/eX",
    "eydxavg" = "eY/dX",
    "dyexavg" = "dY/eX",
    "dydxavgwts" = "dY/dX",
    "eyexavgwts" = "eY/eX",
    "eydxavgwts" = "eY/dX",
    "dyexavgwts" = "dY/eX",
    "ratio" = "%s / %s",
    "ratioavg" = "mean(%s) / mean(%s)",
    "ratioavgwts" = "mean(%s) / mean(%s)",
    "lnratio" = "ln(%s / %s)",
    "lnratioavg" = "ln(mean(%s) / mean(%s))",
    "lnratioavgwts" = "ln(mean(%s) / mean(%s))",
    "lnor" = "ln(odds(%s) / odds(%s))",
    "lnoravg" = "ln(odds(%s) / odds(%s))",
    "lnoravgwts" = "ln(odds(%s) / odds(%s))",

    # Keep %s placeholders so categorical contrasts retain lo/hi labels.
    "lift" = "lift(%s, %s)",
    "liftavg" = "lift(%s, %s)",
    "liftavgwts" = "lift(%s, %s)",
    # All three must carry the label: get_comparisons_data_numeric() keys the
    # eps-step derivative contrast off it, and without an entry the averaged
    # variants silently fell back to the default "+1" unit contrast.
    "expdydx" = "exp(dY/dX)",
    "expdydxavg" = "exp(dY/dX)",
    "expdydxavgwts" = "exp(dY/dX)"
)

sanity_comparison <- function(comparison) {
    # wts versions are used internally but not available directly to users
    valid <- names(comparison_function_dict)
    valid <- valid[!grepl("wts$", valid)]
    checkmate::assert(
        checkmate::check_choice(comparison, choices = valid),
        checkmate::check_function(comparison)
    )
}


sanitize_transform <- function(x) {
    good <- c("exp", "ln")
    # issue #1115: sanitize_transform() wraps `transform` into a named list, so the assertion may fail when using `inferences()`
    if (isTRUE(checkmate::check_list(x, names = "named"))) {
        checkmate::assert(
            checkmate::check_choice(x[[1]], choices = good, null.ok = TRUE),
            checkmate::check_function(x[[1]])
        )
        x <- x[[1]]
    } else {
        checkmate::assert(
            checkmate::check_choice(x, choices = good, null.ok = TRUE),
            checkmate::check_function(x)
        )
    }

    if (is.null(x)) {
        return(x)
    }

    if (is.function(x)) {
        out <- list(x)
        names(out) <- deparse(substitute(x))
    } else if (x == "exp") {
        out <- list("exp" = exp)
    } else if (x == "ln") {
        out <- list("ln" = log)
    }

    return(out)
}
