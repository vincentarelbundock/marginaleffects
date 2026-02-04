using("marginaleffects")
source("helpers.R")

requiet("multilevLCA")

# Single-level multiLCA model, three different estimators (no validity test)
mlca_dat <- multilevLCA::dataTOY
testY <- colnames(mlca_dat)[2:8]
testZ <- c("Z_low", colnames(mlca_dat)[9:10])
testvars <- c("Y_8", "Z_low")

out_onestep = multiLCA(
  mlca_dat, testY, 3, Z = testZ, fixedpars = 0, verbose = FALSE, extout = TRUE
)
expect_predictions(out_onestep, newdata = head(mlca_dat), variables = testvars)
expect_comparisons(out_onestep, newdata = head(mlca_dat), variables = testvars)

out_twostep = multiLCA(
  mlca_dat, testY, 3, Z = testZ, fixedpars = 1, verbose = FALSE, extout = TRUE
)
expect_predictions(out_twostep, newdata = head(mlca_dat), variables = testvars)
expect_comparisons(out_twostep, newdata = head(mlca_dat), variables = testvars)

out_twostage = multiLCA(
  mlca_dat, testY, 3, Z = testZ, fixedpars = 2, verbose = FALSE, extout = TRUE
)
expect_predictions(out_twostage, newdata = head(mlca_dat), variables = testvars)
expect_comparisons(out_twostage, newdata = head(mlca_dat), variables = testvars)

# rowid propagation
nd_rowid <- head(mlca_dat, 3)
nd_rowid$rowid <- c(101, 103, 102)
pred_rowid <- predictions(out_onestep, newdata = nd_rowid)
expect_equivalent(sort(unique(pred_rowid$rowid)), sort(nd_rowid$rowid))

# class labels are consistent
pred_groups <- unique(predictions(out_onestep, newdata = head(mlca_dat, 2))$group)
expected_groups <- colnames(out_onestep$mPhi)
expect_equivalent(sort(pred_groups), expected_groups)
