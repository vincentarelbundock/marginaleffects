source(here::here("tests/r/load.R"))

# Formula-hypothesis shortcuts: trt_vs_ctrl, meandev, meanotherdev, poly,
# helmert. Read the same vendored CSV as tests/test_hypothesis_shortcuts.py.
dat <- fread(here("tests/data/mtcars.csv"), na.strings = c("NA", ""))
mod <- lm(mpg ~ hp + wt + am, data = dat)

ungrouped <- c(
    "~ trt_vs_ctrl",
    "ratio ~ trt_vs_ctrl",
    "~ meandev",
    "ratio ~ meandev",
    "~ meanotherdev",
    "ratio ~ meanotherdev",
    "~ poly",
    "dotproduct ~ poly",
    "~ helmert",
    # pairwise order: lower/upper triangle in column-major order
    "~ pairwise",
    "ratio ~ revpairwise"
)
grouped <- c(
    "~ trt_vs_ctrl | am",
    "~ meandev | am",
    "~ meanotherdev | am",
    "~ poly | am",
    "~ helmert | am",
    # Grouped labels for the original shortcuts, which share the label code
    "~ reference | am",
    "ratio ~ pairwise | am"
)

run <- function(spec, by) {
    out <- avg_predictions(mod, by = by, hypothesis = stats::as.formula(spec))
    out <- data.table::as.data.table(out)
    if (!"am" %in% colnames(out)) out[, am := NA_integer_]
    out[, .(spec = spec, am, hypothesis, estimate, std.error)]
}

out <- rbind(
    rbindlist(lapply(ungrouped, run, by = "carb")),
    rbindlist(lapply(grouped, run, by = c("am", "carb")))
)
fwrite(out, here("tests/r/test_hypothesis_shortcuts.csv"))
