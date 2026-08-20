source(here::here("tests/r/load.R"))

# Read the same vendored CSV as tests/test_hypotheses_joint.py.
mtcars_dat <- fread(here("tests/data/mtcars.csv"), na.strings = c("NA", ""))

mod <- lm(am ~ hp + wt + disp, data = mtcars_dat)
mod_without_intercept <- lm(am ~ 0 + hp + wt + disp, data = mtcars_dat)

# `joint` indices are 0-based in Python and 1-based in R, so the R positions
# below are the Python positions in tests/test_hypotheses_joint.py plus one.
save <- function(x, file) {
    write.csv(x, here(file.path("tests/r", file)), row.names = FALSE)
}

save(hypotheses(mod, joint = c("hp", "wt")), "test_hypotheses_joint_01.csv")
save(
    hypotheses(mod, joint = c("hp", "disp"), joint_test = "chisq"),
    "test_hypotheses_joint_02.csv"
)
save(hypotheses(mod, joint = c(2, 3)), "test_hypotheses_joint_03.csv")
save(
    hypotheses(mod, joint = c(1, 2, 3), hypothesis = c(1, 2, 3)),
    "test_hypotheses_joint_04.csv"
)
save(
    hypotheses(mod, joint = c("(Intercept)", "disp", "wt"), hypothesis = 4),
    "test_hypotheses_joint_05.csv"
)
save(
    hypotheses(mod_without_intercept, joint = c(1, 2, 3)),
    "test_hypotheses_joint_06.csv"
)
save(
    hypotheses(mod_without_intercept, joint = c("hp", "wt")),
    "test_hypotheses_joint_07.csv"
)
