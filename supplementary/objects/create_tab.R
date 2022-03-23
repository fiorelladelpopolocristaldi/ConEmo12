# Packages ----------------------------------------------------------------

library(magrittr)
library(readxl)
library(flextable)
library(ftExtra)

# Importing ---------------------------------------------------------------

dat <- readxl::read_xlsx("supplementary/objects/tab-exp-cond.xlsx", skip = 1)

# Creating Table ----------------------------------------------------------

oldnames <- names(tab)
newnames <- stringr::str_remove_all(names(tab), pattern = "_learning|_test")
newnames <- lapply(newnames, function(x) x)
newnames <- set_names(newnames, oldnames)

tab %>% 
  flextable() %>% 
  theme_vanilla() %>% 
  autofit() %>% 
  align(part = "all", align = "center") %>% 
  add_header_row(
    values = c("Group", rep("Learning Phase", 4), rep("Test Phase", 6)), 
    top = TRUE
  ) %>% 
  set_header_labels(values = newnames) %>% 
  merge_h(i = 1, part = "header") %>% 
  merge_v(j = 1, part = "header") %>% 
  merge_at(i = c(1,2), j = c(4)) %>% 
  merge_at(i = c(1,2), j = c(5)) %>%
  merge_at(i = c(3,4), j = c(4)) %>%
  merge_at(i = c(3,4), j = c(5)) %>% 
  merge_v(j = c(1,2,3,6)) %>% 
  vline(j = 5) %>% 
  colformat_md() %>% 
  saveRDS("supplementary/objects/tab_exp_cond.rds")