withr_library("car")

test_that("hypotheses() returns data.frame when FUN and hypotheses are NULL", {
  dat <- mtcars
  mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
  dmm <- hypotheses(mod)
  expect_s3_class(dmm, "data.frame")
})

test_that("Test of equality between coefficients matches car::linearHypothesis", {
  dat <- mtcars
  mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
  dmm <- hypotheses(mod, "hp = wt")
  dmc <- car::linearHypothesis(mod, hypothesis = "hp = wt")
  expect_equal(dmm$estimate, attr(dmc, "value")[[1]], ignore_attr = TRUE)
  expect_equal(dmm$std.error, sqrt(attr(dmc, "vcov")[[1]]), ignore_attr = TRUE)
})

test_that("Non-linear function and robust vcov work", {
  dat <- mtcars
  mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)
  dmm <- hypotheses(mod, "exp(hp + wt) = 0.1")
  expect_s3_class(dmm, "data.frame")

  dmm <- hypotheses(mod, "hp = wt", vcov = "HC3")
  expect_s3_class(dmm, "data.frame")
})

test_that("b* shortcuts and backtick handling", {
  dat <- mtcars
  mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)

  dmm <- hypotheses(mod, "b2 = b3") # first call may warn in some versions
  expect_s3_class(dmm, "data.frame")

  dmm <- hypotheses(mod, "`factor(cyl)6` = `factor(cyl)8`")
  expect_s3_class(dmm, "data.frame")
})

test_that("FUN can compute SEs for fitted values (glm link/response)", {
  mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

  f <- function(x) predict(x, type = "link", newdata = mtcars)
  p <- hypotheses(mod, hypothesis = f)
  expect_s3_class(p, "data.frame")
  expect_true(all(p$std.error > 0))

  f <- function(x) predict(x, type = "response", newdata = mtcars)
  p <- hypotheses(mod, hypothesis = f)
  expect_s3_class(p, "data.frame")
  expect_true(all(p$std.error > 0))
})

test_that("Equality between predictions via custom FUN", {
  mod <- glm(am ~ hp + mpg, data = mtcars, family = binomial)

  fun <- function(x) {
    p <- predict(x, type = "link", newdata = mtcars)
    data.frame(term = "b1 = b2", estimate = p[1] - p[2])
  }
  dmm <- hypotheses(mod, hypothesis = fun)
  expect_equal(dmm$estimate, 0, ignore_attr = TRUE)

  fun <- function(x) {
    p <- predict(x, type = "link", newdata = mtcars)
    data.frame(term = "b3 = b2", estimate = p[3] - p[2])
  }
  dmm <- hypotheses(mod, hypothesis = fun)
  expect_equal(dmm$estimate, 1.33154848763268, ignore_attr = TRUE)
})

test_that("Named matrix of hypotheses", {
  mod <- lm(mpg ~ factor(cyl), data = mtcars)
  hyp <- matrix(
    c(0, -1, 1, 1 / 3, 1 / 3, 1 / 3),
    ncol = 2,
    dimnames = list(NULL, c("H1", "H2"))
  )
  del <- hypotheses(mod, hypothesis = hyp)
  expect_equal(del$term, c("H1", "H2"), ignore_attr = TRUE)
})

test_that("Two-step call: hypotheses() on marginaleffects object", {
  mod <- lm(mpg ~ factor(cyl), data = mtcars)
  cmp <- avg_comparisons(mod)
  hyp <- hypotheses(cmp, equivalence = c(-10, -5))
  expect_s3_class(hyp, "hypotheses")
})

test_that("Issue #656: works with lists and purrr", {
  withr_library("purrr")

  reg_list <- list(
    lm(mpg ~ wt + hp, data = mtcars),
    lm(mpg ~ wt + hp + factor(vs), data = mtcars)
  )
  expect_s3_class(hypotheses(reg_list[[1]]), "hypotheses")
  expect_s3_class(hypotheses(reg_list[[2]]), "hypotheses")

  h <- lapply(reg_list, hypotheses)
  expect_true(is.list(h))
  expect_equal(length(h), 2, ignore_attr = TRUE)

  h <- purrr::map(reg_list, hypotheses)
  expect_true(is.list(h))
  expect_equal(length(h), 2, ignore_attr = TRUE)

  cmp <- lapply(reg_list, comparisons)
  tmp <- lapply(cmp, function(x) hypotheses(x, "b1 = b2"))
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "hypotheses")
  expect_s3_class(tmp[[2]], "hypotheses")

  tmp <- purrr::map(cmp, hypotheses, hypothesis = "b1 = b2")
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "hypotheses")
  expect_s3_class(tmp[[2]], "hypotheses")

  mfx <- lapply(reg_list, avg_slopes)
  tmp <- lapply(mfx, function(x) hypotheses(x, "b1 = b2"))
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "hypotheses")
  expect_s3_class(tmp[[2]], "hypotheses")

  tmp <- purrr::map(mfx, hypotheses, hypothesis = "b1 = b2")
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "hypotheses")
  expect_s3_class(tmp[[2]], "hypotheses")

  pre <- lapply(reg_list, predictions)
  tmp <- lapply(pre, function(x) hypotheses(x, "b1 = b2"))
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "hypotheses")
  expect_s3_class(tmp[[2]], "hypotheses")

  tmp <- purrr::map(pre, hypotheses, hypothesis = "b1 = b2")
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "hypotheses")
  expect_s3_class(tmp[[2]], "hypotheses")

  tmp <- purrr::map(reg_list, ~ hypotheses(.) |> tidy())
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "tbl_df")
  expect_s3_class(tmp[[2]], "tbl_df")

  tmp <- purrr::map(reg_list, function(reg) {
    reg |>
      hypotheses("wt = 0") |>
      broom::tidy()
  })
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "tbl_df")
  expect_s3_class(tmp[[2]], "tbl_df")

  tmp <- purrr::map(reg_list, function(reg) tidy(hypotheses(reg, "wt = 0")))
  expect_true(is.list(tmp))
  expect_s3_class(tmp[[1]], "tbl_df")
  expect_s3_class(tmp[[2]], "tbl_df")
})

test_that("Issue #776: sort-before-hypothesis equivalence", {
  load("modelarchive/data/gusto.rda")
  mod <- glm(
    day30 ~ tx * sex + age,
    family = "binomial",
    data = gusto
  )
  cmp <- avg_comparisons(
    mod,
    type = "link",
    variables = list("tx" = "pairwise"),
    by = "sex"
  )
  x <- hypotheses(cmp, hypothesis = "b4 - b3 = 0")
  y <- cmp$estimate[4] - cmp$estimate[3]
  z <- avg_comparisons(
    mod,
    type = "link",
    variables = list("tx" = "pairwise"),
    by = "sex",
    hypothesis = "b4 - b3 = 0"
  )
  expect_equal(x$estimate, y, ignore_attr = TRUE)
  expect_equal(z$estimate, y, ignore_attr = TRUE)
})

test_that("Labels and custom labels", {
  dat <- mtcars
  mod <- lm(mpg ~ hp + wt + factor(cyl), data = dat)

  hyp <- hypotheses(mod, hypothesis = "b* = b2")
  known <- c("b1=b2", "b2=b2", "b3=b2", "b4=b2", "b5=b2")
  expect_true(all(hyp$hypothesis %in% known))

  hyp <- hypotheses(mod, hypothesis = c("equal" = "b1 = b2", "sums to zero" = "wt + hp = 0"))
  expect_equal(hyp$hypothesis, c("equal", "sums to zero"), ignore_attr = TRUE)

  hyp <- hypotheses(mod, hypothesis = c("equal" = "b*=0"))
  expect_equal(hyp$hypothesis, sprintf("b%s=0", 1:5), ignore_attr = TRUE)
})

test_that("Issue #960: safety warnings and df override", {
  withr_library("nlme")
  fm1 <- lme(distance ~ age + Sex, data = Orthodont)
  
  # Enable safety warnings for this test
  withr::local_options(list(marginaleffects_safe = TRUE))
  expect_warning(
    hypotheses(fm1, hypothesis = c(0, 0), joint = c("SexFemale", "age")))

  h <- hypotheses(fm1, hypothesis = c(0, 0), df = c(1, 3), joint = c("SexFemale", "age"))
  expect_s3_class(h, "hypotheses")
})

test_that("Issue #1344: hypotheses with custom contrast function in formula", {
  mod <- lm(mpg ~ cyl * am * hp, mtcars)

  # keep global to allow evaluation in model env when parsing the formula
  helmert <<- function(x) {
    w <- contr.helmert(length(x))
    setNames(
      as.vector(x %*% w),
      nm = paste0("h-", seq_len(ncol(w)))
    )
  }

  h <- avg_slopes(mod, variables = c("hp"), by = c("cyl", "am"), newdata = "balanced")
  h <- hypotheses(h, hypothesis = ~ I(helmert(x)) | am)
  expect_true("am" %in% colnames(h))
  expect_s3_class(h, "hypotheses")
})

test_that("Sequential calls yield consistent estimates and SEs", {
  mod <- lm(mpg ~ factor(cyl), mtcars)

  p1 <- predictions(mod, by = "cyl", hypothesis = c("(b3 - b2) - (b2 - b1) = 0"))

  p2 <- predictions(mod, by = "cyl", hypothesis = c("b2 - b1 = 0", "b3 - b2 = 0"))
  p2 <- hypotheses(p2, "b2 - b1 = 0")

  p3 <- predictions(mod, by = "cyl")
  p3 <- hypotheses(p3, hypothesis = c("b2 - b1 = 0", "b3 - b2 = 0"))
  p3 <- hypotheses(p3, "b2 - b1 = 0")

  expect_equal(p1$estimate, p2$estimate, ignore_attr = TRUE)
  expect_equal(p1$estimate, p3$estimate, ignore_attr = TRUE)
  expect_equal(p1$std.error, p2$std.error, ignore_attr = TRUE)
  expect_equal(p1$std.error, p3$std.error, ignore_attr = TRUE)
})
