test_that("mlogit package works", {
    skip("TODO: broken")
    skip_if_not_installed("mlogit")
    skip_if_not_installed("dplyr")

    withr_library("mlogit")
    withr_library("dplyr")

    # Test custom hypothesis functions with mlogit
    data("Fishing", package = "mlogit")
    Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")
    m <- mlogit(mode ~ price + catch | income, data = Fish)

    # Create contrast data
    lo <- transform(as.data.frame(Fish), term = "lo")
    lo$id1 <- Fish$idx$id1
    lo$id2 <- Fish$idx$id2
    lo$idx <- NULL
    hi <- transform(lo, price = ifelse(id2 == "beach", price + 100, price), term = "hi")
    dat <- rbind(lo, hi)
    dat$term <- factor(dat$term, levels = c("lo", "hi"))

    # Test average probability predictions
    p <- predictions(m, newdata = dat, by = c("group", "term"))
    expect_s3_class(p, "predictions")
    expect_true(all(c("beach", "boat", "charter", "pier") %in% p$group))
    expect_true(all(c("lo", "hi") %in% p$term))
    expect_true(all(p$estimate > 0 & p$estimate < 1))

    # Test custom hypothesis function for average probabilities
    h <- function(x) {
        x |> summarize(estimate = mean(estimate), .by = c("term", "group"))
    }
    p_custom <- predictions(m, newdata = dat, hypothesis = h)
    expect_s3_class(p_custom, "predictions")
    expect_equal(p$estimate, p_custom$estimate, tolerance = 1e-5, ignore_attr = TRUE)

    # Test custom hypothesis function for marginal effects
    h_mfx <- function(x) {
        x |>
            summarize(estimate = mean(estimate), .by = c("term", "group")) |>
            summarize(estimate = diff(estimate), .by = "group") |>
            rename(term = group)
    }
    mfx <- predictions(m, newdata = dat, hypothesis = h_mfx)
    expect_s3_class(mfx, "predictions")
    expect_true(all(c("beach", "boat", "charter", "pier") %in% mfx$term))
    expect_true(all(mfx$estimate[1] < 0)) # beach probability should decrease
    expect_true(all(mfx$estimate[2:4] > 0)) # other modes should increase

    expect_error(avg_comparisons(m), pattern = "predictions.*supported")
    expect_error(avg_slopes(m), pattern = "predictions.*supported")
})
