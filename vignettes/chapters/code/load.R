library(knitr)
library(tinytable)
library(marginaleffects)
library(modelsummary) |> suppressPackageStartupMessages()
library(ggplot2)
library(data.table)
library(basetheme)
basetheme("clean")

# print
source("code/print.summary.lm.R")
source("code/print.summary.glm.R")
source("code/print.summary.polr.R")


# marginaleffects
options(marginaleffects_print_style = "tinytable")
options(marginaleffects_print_type = FALSE)
options(marginaleffects_print_omit = "s.value")
options(marginaleffects_print_column_names = FALSE)


# tinytable
options(tinytable_latex_preamble = FALSE) # avoid duplication with preamble.sty
options(tinytable_theme_placement_latex_float = "H")
fun <- function(x) {
    out <- x |> 
        theme_tt("resize", direction = "down") |>
        format_tt(j = "agecat", sprintf = "{%s}") |>
        style_tt(tabularray_inner = "cells={font=\\footnotesize}")
    return(out)
}
options(tinytable_tt_theme = fun)


# modelsummary
tidy_custom.comparisons <- function(x) {
    if ("contrast" %in% colnames(x)) {
        x$contrast <- gsub("mean\\(", "", x$contrast)
        x$contrast <- gsub("\\)", "", x$contrast)
    }
    x <- as.data.frame(lapply(x, as.vector))
    return(x)
}


# ggplot2
theme_vab <- function(x) {
    theme_bw() + theme(
        strip.background = element_blank(),
        panel.grid = element_blank()
    ) +
    theme(panel.spacing.x = unit(5, "mm"))
}
theme_set(theme_vab())
okabeito <- c('#E69F00', '#56B4E9', '#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7', '#999999', '#000000')
options(ggplot2.discrete.fill = okabeito)
options(ggplot2.discrete.colour = okabeito)

knitr::opts_chunk$set(
    out.width = "70%", 
    fig.width = 6, 
    fig.asp = 0.618, 
    fig.align = "center"
)

def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
    x <- def.chunk.hook(x, options)
    paste0("\n \\", "small","\n\n", x, "\n\n \\normalsize")
})

out2fig = function(out.width, out.width.default = 0.7, fig.width.default = 6) {
    fig.width.default * out.width / out.width.default 
}


