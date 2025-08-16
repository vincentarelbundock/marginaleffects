exit_file("should survfit be supported?")
source("helpers.R")
using("marginaleffects")
requiet("systemfit")
requiet("MASS")

# issue #1233: duplicated coefficient names
set.seed(12345)
N <- 100
C <- matrix(c(1, .3, .3, .3, 1, .3, .3, .3, 1), 3, 3)
colnames(C) <- rownames(C) <- c("x1", "x2", "x3")
data <- as.data.frame(mvrnorm(N, rep(0, 3), C))
sys <- with(data, {
    y1 <- 2 * x1 + 3 * x2 + x3 + 10 * rnorm(N)
    m1 <- lm(y1 ~ x1 + x2 + x3)
    y2 <- x1 + 3 * x2 + 2 * x3 + 10 * rnorm(N)
    m2 <- lm(y2 ~ x1 + x2 + x3)
    systemfit(list(formula(m1), formula(m2)), data = data)
})
h <- hypotheses(sys)
expect_false(anyNA(h$std.error))
