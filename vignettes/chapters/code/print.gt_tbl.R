library(knitr)
knit_print.gt_tbl <- function (x, options, ...) {
    tab <- modelsummary::clean_latex(x, label = options$label) %>%
           str_split('\n') %>% unlist() %>%
           .[!str_detect(., 'captionsetup')] %>%
           str_replace('longtable', 'tabu')
    idx <- grep('caption', tab)
    cap <- tab[idx:(idx+2)] %>% 
           str_replace_all(fixed('\\'), '') %>%
           str_trim %>%
           paste(collapse = '') %>%
           paste0('\\', .) %>%
           str_replace('label\\{', '\\\\label{') %>%
           str_replace_all('toprule', '')
    tab <- tab[-(idx:(idx+2))]
    #tab <- str_replace(tab, '(.*begin.*tabu.*)', '\\1\n')#\\\\toprule')
    tab <- str_replace(tab, '(.*begin.*tabu.*)', '\\1\n\\\\toprule')
    tab <- c('\\begin{table}', cap, tab, '\\end{table}') %>%
           paste(collapse = '\n')
    tab <- str_replace(tab, 'Num.Obs.', '\\midrule\nNum.Obs') # midrule to split coef/gof
    tab <- tab[tab != '']
    asis_output(tab)
}
registerS3method("knit_print", "gt_tbl", knit_print.gt_tbl)


#library(tidyverse)
#library(gt)
#rnorm(4) %>% matrix(ncol=2) %>% data.frame %>% 
    #gt() %>% 
    #tab_header(title = 'Junk ttitle') %>%
    #knit_print(options = list('label'='test')) %>%
    ##str_replace('label\\{', '\\\\label{') %>%
    #cat
