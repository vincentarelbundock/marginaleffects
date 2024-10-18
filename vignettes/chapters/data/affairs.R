library(data.table)
dat <- fread("https://vincentarelbundock.github.io/Rdatasets/csv/AER/Affairs.csv")
dat[, affairs := fcase(affairs == 0, "0",
                       affairs == 1, "1",
                       affairs == 2, "2",
                       affairs == 3, "3",
                       affairs == 7, "4-10",
                       affairs == 12, ">10")]
dat[, rownames := NULL]
setnames(dat, old = "rating", "happy")
setkey(dat, affairs)
fwrite(dat, "chapters/data/affairs.csv")

