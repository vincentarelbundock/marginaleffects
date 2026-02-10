source("helpers.R")
using("marginaleffects")

requiet("DirichletReg")

data("ArcticLake", package = "DirichletReg")
ALake <- ArcticLake
ALake$AL <- DirichletReg::DR_data(ArcticLake[, 1:3]) |>
    suppressWarnings()
mod1 <- DirichletReg::DirichReg(
    AL ~ depth + I(depth^2) | depth,
    data = ALake,
    model = "alternative"
) |> suppressWarnings()

op <- getOption("marginaleffects_safe", default = TRUE)
warn_op <- getOption("warn")
options(marginaleffects_safe = TRUE, warn = 1)
on.exit(options(marginaleffects_safe = op, warn = warn_op), add = TRUE)

expect_warning(
    predictions(mod1),
    pattern = "DirichletReg model support is experimental"
)
expect_warning(
    avg_slopes(mod1),
    pattern = "DirichletReg model support is experimental"
)

expect_predictions(mod1)
expect_slopes(mod1)

avg <- avg_slopes(mod1)
expect_inherits(avg, "slopes")
expect_false(anyNA(avg$estimate))
expect_equal(nrow(avg), 3)
