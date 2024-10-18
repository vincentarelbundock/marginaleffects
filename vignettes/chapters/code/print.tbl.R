library(knitr)
knit_print.tbl <- function (x, options, ...) {
    out <- data.frame(x)
    print(out)
}
registerS3method("knit_print", "tbl", knit_print.tbl)
