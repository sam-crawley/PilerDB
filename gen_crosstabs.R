library(tidyverse)
library(janitor)
library(writexl)

gen.wvs.crosstabs <- function() {
  data <- readRDS("Divided/data/orig/WVS_Cross-National_Wave_7_R_v1_6.rds")
  
  res <- map(unique(data$B_COUNTRY_ALPHA), function(country) {
    
    d <- data %>%
      filter(B_COUNTRY_ALPHA == country) %>%
      mutate(across(c(Q223, Q272, Q289, Q290), haven::as_factor)) %>%
      mutate(across(c(Q223, Q272, Q289, Q290), fct_explicit_na)) %>%
      mutate(across(c(Q223, Q272, Q289, Q290), ~fct_lump_prop(.x, 0.05)))
    
    map(list("Q272", "Q289", "Q290"), function(var) {
      d %>% 
        tabyl(Q223, .data[[var]], show_missing_levels = F) %>% 
        adorn_percentages("all") %>% 
        adorn_pct_formatting() %>% 
        adorn_ns()
    })
    
  })
  
  res <- flatten(res)
  
  names <- expand_grid(unique(data$B_COUNTRY_ALPHA), c("Language", "Relgion", "Ethnicity")) %>% 
    setNames(c("Country", "Type")) %>% 
    unite("label", Country, Type, sep = " ")
  
  names(res) <- names$label
  
  write_xlsx(res, path = "Divided/data/output/WVS_crosstabs.xls", col_names = TRUE, format_headers = TRUE, use_zip64 = FALSE)  
  
  return(res)
  
}
