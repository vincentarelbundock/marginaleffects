skip_if_not_installed("speedglm")
requiet("speedglm")

test_that("speedglm: no validity check", {
    set.seed(10)
    n <- 500
    k <- 10
    y <- rgamma(n,1.5,1)
    x <- round( matrix(rnorm(n*k),n,k),digits=3)
    colnames(x) <- paste("s",1:k,sep = "")
    da <- data.frame(y,x)
    fo <- as.formula(paste("y~",paste(paste("s",1:k,sep=""),collapse="+")))   
    model <- speedglm(fo,data=da,family=Gamma(log))
    mfx <- marginaleffects(model)
    expect_s3_class(mfx, "data.frame")
    expect_true(!any(mfx$estimate == 0))
    expect_true(!any(mfx$std.error == 0))
})
