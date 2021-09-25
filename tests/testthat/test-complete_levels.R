requiet("dplyr")

test_that("padding with interactions", {
    skip("reinstate test when type is supported by `predictions`")
    dat <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/ggplot2movies/movies.csv") %>%
           mutate(style = case_when(Action == 1 ~ "Action",
                                    Comedy == 1 ~ "Comedy",
                                    Drama == 1 ~ "Drama",
                                    TRUE ~ "Other"),
                  style = factor(style),
                  certified_fresh = rating >= 8) %>%
           filter(length < 240)
    mod <- glm(certified_fresh ~ length * style, data = dat, family = binomial)
    res <- predictions(mod, type = c("response", "link"))
    expect_false(anyNA(res$style))
    expect_equal(nrow(res), 2)
})
