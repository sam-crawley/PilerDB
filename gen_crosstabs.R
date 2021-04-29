library(tidyverse)
library(janitor)
library(openxlsx)
library(countrycode)

skip.countries <- c("CHN", "EGY", "VNM")

gen.wvs.crosstabs <- function(write.res = T) {
  data <- readRDS("Divided/data/orig/WVS_Cross-National_Wave_7_R_v1_6.rds") %>%
    filter(! B_COUNTRY_ALPHA %in% skip.countries)
  
  res <- map(unique(data$B_COUNTRY_ALPHA), function(country) {
    
    d <- data %>%
      filter(B_COUNTRY_ALPHA == country) %>%
      mutate(across(c(Q223, Q272, Q289, Q290), haven::as_factor)) %>%
      mutate(across(c(Q223, Q272, Q289, Q290), fct_explicit_na)) %>%
      mutate(across(c(Q223, Q272, Q289, Q290), ~fct_lump_prop(.x, 0.05)))
    
    tables <- map(list("Q272", "Q289", "Q290"), function(var) {
      d %>% 
        tabyl(Q223, .data[[var]], show_missing_levels = F) %>% 
        adorn_percentages("row") %>% 
        adorn_pct_formatting() %>% 
        adorn_ns() %>%
        rename("Party" = Q223)
    })
    
    names(tables) <- c("Language", "Relgion", "Ethnicity")
    
    tables
    
  })

  names(res) <- countrycode(unique(data$B_COUNTRY_ALPHA), origin = 'iso3c', destination = 'country.name')

  if (write.res)
    write.wvs.xlsx(res)
  
  return(res)
  
}

write.wvs.xlsx <- function(res) {
  wb <- createWorkbook()
  
  hs1 <- createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- createStyle(textDecoration = "bold")
  
  for (country in names(res)) {
    addWorksheet(wb, country)
    
    startRow <- 1
    
    for (table.name in names(res[[country]])) {
      table <- res[[country]][[table.name]]
      
      writeData(wb, country, table.name, startCol = 1, startRow = startRow, rowNames = F)
      addStyle(wb, sheet = country, hs1, rows = startRow, cols = 1)
      
      writeData(wb, country, table, startCol = 1, startRow = startRow + 1, rowNames = F, headerStyle = hs2)
      setColWidths(wb, sheet = country, cols = 1:ncol(table), widths = "auto")
      
      startRow <- nrow(table) + 3 + startRow
    }
  }
  
  saveWorkbook(wb, "Divided/data/output/wvs_crosstabs.xlsx", overwrite = T)
  
}
