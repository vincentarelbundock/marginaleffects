source("helpers.R")
using("marginaleffects")
requiet("Amelia")

# Basic expectation tests
dat_simple <- mtcars
dat_simple$mpg[1:5] <- NA
amelia_obj <- Amelia::amelia(dat_simple, m = 5, p2s = 0)
mod_simple <- with(amelia_obj, lm(mpg ~ wt + am))
expect_slopes(mod_simple)
expect_predictions(mod_simple)
expect_hypotheses(mod_simple)
expect_comparisons(mod_simple)

dat <- iris
dat$Sepal.Length[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Sepal.Width[sample(seq_len(nrow(iris)), 40)] <- NA
dat$Species[sample(seq_len(nrow(iris)), 40)] <- NA
dat_amelia <- Amelia::amelia(dat, m = 20, noms = "Species", p2s = 0)
amest <- with(dat_amelia, lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species))
mod <- lm(Petal.Width ~ Sepal.Length * Sepal.Width + Species, data = dat)

mfx1 <- suppressWarnings(avg_slopes(amest, by = "Species"))
mfx2 <- avg_slopes(mod, by = "Species")
expect_inherits(mfx1, "slopes")
expect_equivalent(nrow(mfx1), nrow(mfx2))


# Issue #711
data <- structure(
    list(
        id = 1:37,
        trt = c(
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "soc",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm",
            "arm"
        ),
        endp = structure(
            c(
                1L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                1L,
                2L,
                2L,
                2L,
                2L,
                2L,
                2L,
                2L,
                1L,
                1L,
                1L,
                1L,
                1L,
                1L,
                1L,
                2L,
                2L,
                2L,
                2L,
                2L,
                1L,
                1L,
                1L,
                1L
            ),
            levels = c("TRUE", "FALSE"),
            class = "factor"
        )
    ),
    row.names = c(
        NA,
        -37L
    ),
    class = "data.frame"
)
data$endp <- factor(data$endp, levels = c("TRUE", "FALSE"))
data_miss <- data
data_miss[c(1, 5, 7, 30), c("endp")] <- NA
imp <- suppressWarnings(Amelia::amelia(data_miss, m = 20, noms = c("trt", "endp"), p2s = 0))
dat_amelia <- imp$imputations
fit_logistic <- function(dat) {
    mod <- glm(endp ~ trt, family = binomial(link = "logit"), data = dat)
    out <- avg_slopes(mod, newdata = dat)
    return(out)
}
mod_imputation <- suppressWarnings(lapply(dat_amelia, fit_logistic))
manu <- suppressWarnings(summary(mice::pool(mod_imputation), conf.int = TRUE))
fit <- with(imp, glm(endp ~ trt, family = binomial(link = "logit")))
auto <- suppressWarnings(avg_slopes(fit))
expect_equivalent(auto$estimate, manu$estimate)
expect_equivalent(auto$std.error, manu$std.error, tolerance = 1e-5)
