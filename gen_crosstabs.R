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
    
    for (var in c("Q223", "Q272", "Q289", "Q290")) {
      levels(d[[var]]) <- str_remove(levels(d[[var]]), "^\\w+:\\s*")
    }
    
    tables <- map(list("Q272", "Q289", "Q290"), function(var) {
      setClass <- function(i) { class(i) <- 'percentage'; i} 
      
      t <- d %>% 
        tabyl(Q223, .data[[var]], show_missing_levels = F) %>% 
        adorn_percentages("row") %>% 
        rename("Party" = Q223) %>% 
        mutate(across(-Party, ~setClass(.)))
      
      t <- inner_join(t, attr(t, "core") %>% rename("Party" = Q223), by = "Party", suffix = c(".%", ".n"))
    })
    
    names(tables) <- c("Language", "Relgion", "Ethnicity")
    
    tables[['Sample Size']] <- nrow(d)
    
    tables
    
  }) %>%
    set_names(
      data %>% select(B_COUNTRY_ALPHA, A_YEAR) %>% distinct() %>% 
        mutate(Country = countrycode(B_COUNTRY_ALPHA, origin = 'iso3c', destination = 'country.name')) %>%
        unite("Tab.Name", Country, A_YEAR, sep = " ") %>%
        pull(Tab.Name)
    )
  
  if (write.res)
    write.wvs.xlsx(res)
  
  return(res)
  
}

write.wvs.xlsx <- function(res) {
  options("openxlsx.numFmt" = NULL)
  wb <- createWorkbook()
  
  hs1 <- createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- createStyle(textDecoration = "bold")
  
  for (country in names(res)) {
    addWorksheet(wb, country)
    
    startRow <- 1
    
    for (table.name in c("Language", "Relgion", "Ethnicity")) {
      table <- res[[country]][[table.name]]
      
      writeData(wb, country, table.name, startCol = 1, startRow = startRow, rowNames = F)
      addStyle(wb, sheet = country, hs1, rows = startRow, cols = 1)
      
      headers <- unique(str_remove(names(table), '\\..$'))
      headerCol <- 1
      for (header in headers) {
        writeData(wb, country, header, startCol = headerCol, startRow = startRow + 1)
        addStyle(wb, sheet = country, hs2, rows = startRow + 1, cols = headerCol)
        
        if (header != "Party") {
          mergeCells(wb, country, cols = headerCol:(headerCol+1), rows = startRow + 1)
          headerCol <- headerCol+1
        }
        
        headerCol <- headerCol+1
      }
      
      cols <- unlist(map(headers, function(h) {
        if (h == "Party")
          return (h)
        
        paste0(h, c(".%", ".n"))
      }))
      
      writeData(wb, country, table %>% select(all_of(cols)), startCol = 1, startRow = startRow + 2, rowNames = F, colNames = F)
      setColWidths(wb, sheet = country, cols = 1, widths = "auto")
      setColWidths(wb, sheet = country, cols = 2:ncol(table), widths = "10")
      
      startRow <- nrow(table) + 3 + startRow
    }
    
    writeData(wb, country, "Sample Size", startCol = 1, startRow = startRow)
    addStyle(wb, sheet = country, hs1, rows = startRow, cols = 1)
    
    writeData(wb, country, res[[country]][['Sample Size']], startCol = 1, startRow = startRow+1)
  }
  
  saveWorkbook(wb, "Divided/data/output/wvs_crosstabs.xlsx", overwrite = T)
  
}
