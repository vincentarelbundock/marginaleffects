test_that("counterfactual functionality works", {
    # old bug: counterfactual with a single regressor
    mod <- lm(mpg ~ hp + drat + wt, mtcars)
    x <- datagrid(model = mod, hp = c(100, 110), grid_type = "counterfactual")
    expect_equal(nrow(x), 64, ignore_attr = TRUE)
    mod <- lm(mpg ~ hp, mtcars)
    x <- datagrid(model = mod, hp = c(100, 110), grid_type = "counterfactual")
    expect_equal(nrow(x), 64, ignore_attr = TRUE)

    # marginal effects does not overwrite counterfactual rowid
    mod <- glm(am ~ mpg + factor(cyl), data = mtcars, family = binomial)
    mfx <- slopes(mod, newdata = datagrid(cyl = c(4, 6, 8), grid_type = "counterfactual"))
    expect_true(all(mfx$rowidcf %in% 1:32))
    expect_true(all(mfx$rowid %in% 1:96))

    # alternative syntaxes
    mod <- lm(mpg ~ hp + drat + wt, mtcars)
    nd1 <- datagrid(wt = 3, hp = c(100, 110), model = mod)
    nd2 <- datagrid(wt = 3, hp = c(100, 110), drat = 4, newdata = mtcars)
    x <- slopes(mod, newdata = datagrid(wt = 3, hp = c(100, 110)))
    x$mpg <- NULL # placeholder response in predictions
    y <- slopes(mod, newdata = nd1)
    z <- slopes(mod, newdata = nd2)
    expect_equal(x$estimate, y$estimate, ignore_attr = TRUE)
    expect_equal(x$conf.low, y$conf.low, tolerance = 1e-5, ignore_attr = TRUE)
    expect_equal(x$estimate, z$estimate, ignore_attr = TRUE)
    expect_equal(x$conf.low, z$conf.low, tolerance = 1e-2, ignore_attr = TRUE)

    # size
    mod <- lm(mpg ~ hp + drat + wt, mtcars)
    expect_equal(
        nrow(datagrid(wt = 2:3, newdata = mtcars, grid_type = "counterfactual")),
        nrow(mtcars) * 2,
        ignore_attr = TRUE
    )
    expect_equal(nrow(datagrid(wt = 2:3, newdata = mtcars)), 2, ignore_attr = TRUE)
    expect_equal(
        nrow(datagrid(wt = 2:3, model = mod, grid_type = "counterfactual")),
        nrow(mtcars) * 2,
        ignore_attr = TRUE
    )
    expect_equal(nrow(datagrid(wt = 2:3, model = mod)), 2, ignore_attr = TRUE)

    # warning on bad `at` entry
    expect_warning(datagrid(newdata = mtcars, at = list("blah" = 0:1), grid_type = "counterfactual"))
    expect_warning(datagrid(newdata = mtcars, at = list("blah" = 0:1)))

    # datagrid(): factor, logical, automatic variable
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    mod <- lm(mpg ~ hp * wt + am + gear, data = tmp)
    res <- datagrid(
        model = mod,
        hp = c(100, 110),
        gear = c(3, 4),
        am = TRUE,
        grid_type = "counterfactual"
    )
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(128, 6), ignore_attr = TRUE)

    # datagrid(): factor, logical, numeric
    tmp <- mtcars
    tmp$am <- as.logical(tmp$am)
    tmp$gear <- as.factor(tmp$gear)
    res <- datagrid(newdata = tmp)
    expect_s3_class(res, "data.frame")
    expect_equal(dim(res), c(1, 12), ignore_attr = TRUE)
    expect_equal(sum(sapply(res, is.logical)), 1, ignore_attr = TRUE)
    expect_equal(sum(sapply(res, is.factor)), 1, ignore_attr = TRUE)
    expect_equal(sum(sapply(res, is.numeric)), 10, ignore_attr = TRUE)

    # typical number of rows
    mod <- lm(mpg ~ hp * wt, data = mtcars)
    nd <- datagrid(model = mod, hp = c(100, 110))
    expect_equal(nrow(slopes(mod, newdata = nd)), 4, ignore_attr = TRUE)
})
