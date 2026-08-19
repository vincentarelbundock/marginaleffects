# Writes iris.csv, the single source of truth for the iris models in
# stata/unconditional.do and test-vcov_unconditional-stata.R. Both sides read
# this file, so the two never drift apart.
#
# Stata rejects `.` in variable names, hence the renames. The two binary
# derived variables stand in for what iris lacks:
#
#   wide   = Sepal.Width > 3      binomial outcome
#   heavy  = Petal.Length > 4     factor to contrast
#   scount = round(2 * Sepal.Width)   count outcome, by coarsening
#
# The cutoffs were chosen so that no binomial fit approaches separation (fitted
# probabilities stay inside [0.06, 0.86]). `scount` coarsens Sepal.Width rather
# than a regressor, so the poisson fit stays well conditioned.
#
# Run from this directory:
#   Rscript generate_iris.R

dat <- data.frame(
    slength = iris$Sepal.Length,
    swidth = iris$Sepal.Width,
    plength = iris$Petal.Length,
    pwidth = iris$Petal.Width,
    species = as.integer(iris$Species),
    wide = as.integer(iris$Sepal.Width > 3),
    heavy = as.integer(iris$Petal.Length > 4),
    scount = as.integer(round(iris$Sepal.Width * 2))
)

write.csv(dat, "iris.csv", row.names = FALSE)
