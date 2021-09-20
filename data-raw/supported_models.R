library("gt")

supported_models <- read.csv("data-raw/supported_models.csv", check.names = FALSE, encoding = "utf8")
usethis::use_data(supported_models, overwrite = TRUE)

# gt table
tmp <- supported_models
for (i in nrow(tmp):2) {
    if (tmp$Package[i] == tmp$Package[i-1]) {
        tmp$Package[i] <- ""
    }
}
colnames(tmp) <- c("Package", 
                   "Function", 
                   "dY/dX", 
                   "Std. Error", 
                   "dY/dX ", 
                   "Std. Error ",
                   "dY/dX  ",
                   "Std. Error  ")
idx_green <- lapply(tmp, function(x) x == TRUE)
idx_red <- lapply(tmp, function(x) x == FALSE)
for (i in 3:ncol(tmp)) {
    tmp[[i]] <- ""
}
tab <- gt(tmp) %>%
  tab_spanner(label = "Support", columns = 3:4) %>%
  tab_spanner(label = "Validity: Stata", columns = 5:6) %>%
  tab_spanner(label = "Validity: margins", columns = 7:8) %>%
  tab_style(style = list(cell_borders(sides = c("left", "right"), color = "black", weight = px(1))),
            locations = list(cells_body(columns = 3:ncol(tmp)))) %>%
  tab_style(style = list(cell_borders(sides = c("bottom", "top"), color = "black", weight = px(1))),
            locations = cells_body())
for (i in 3:ncol(tmp)) {
    tab <- tab %>%
    tab_style(style = cell_fill(color = "#c1e1c1"), 
              locations = cells_body(columns = i, rows = (idx_green[[i]] == TRUE))) %>%
    tab_style(style = cell_fill(color = "#ff6961"), 
              locations = cells_body(columns = i, rows = (idx_red[[i]] == TRUE)))
}
gt::gtsave(tab, "man/figures/README-supported_models.png")
