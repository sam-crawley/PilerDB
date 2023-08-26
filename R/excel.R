# Functions to generate Excel spreadsheets of the DB

gen.spreadsheets <- function(piler.data = NULL) {
  if (is.null(piler.data))
    piler.data <- piler
  
  write.divided.xlsx(piler.data, include.crosstabs = F, file = "inst/excel/piler_summary.xlsx")
  write.divided.xlsx(piler.data, include.summary = F, include.group.sizes = F, include.summary.by.group = F)
}

set.class <- function(class.name, i) { class(i) <- class.name; i} 

get.excel.summary.sheet <- function(res) {
  summary.sheet <- res$summary %>%
    mutate(across(ends_with('.pct'), ~set.class('percentage', .))) %>%
    select(-Religion, -Ethnicity, -Language)
  
  return(summary.sheet)
}


write.divided.xlsx <- function(res, include.summary = T, include.summary.by.group = T, include.group.sizes = T, include.crosstabs = T, include.group.sizes.by.group = T,
                               file = "inst/excel/piler_crosstabs.xlsx") {
  options("openxlsx.numFmt" = NULL)
  wb <- openxlsx::createWorkbook()
  
  hs1 <- openxlsx::createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- openxlsx::createStyle(textDecoration = "bold")
  
  openxlsx::addWorksheet(wb, "About")
  
  spreadsheet.contents <- c("Summaries", "Summaries by group", "Group Sizes", "Group sizes by group", "Country Crosstabs")
  contents.included <- c(include.summary, include.summary.by.group, include.group.sizes, include.group.sizes.by.group, include.crosstabs)
  
  about.data <- c(
    paste0("Divided Societies DB v", res$version), 
    paste("Generated", Sys.time()),
    paste("This spreadsheet includes: ", paste0(spreadsheet.contents[contents.included], collapse = ", "))
  )
  
  openxlsx::writeData(wb, "About", about.data, startRow = 1, startCol = 1)
  
  # Add summary sheet
  if (include.summary) {
    write.excel.summary.tab(wb, res$summary)
  }
  
  if (include.summary.by.group) {
    for (group in group.names) {
      write.excel.summary.tab(wb, res$summary.by.group[[group]], tab.name = paste("Summary -", group))
    }
  }
  
  # Add group sizes sheet
  if (include.group.sizes) {
    write.excel.group.sizes.tab(wb, res$group.sizes)
    
    if (include.group.sizes.by.group) {
      for (group in group.names) {
        write.excel.group.sizes.tab(wb, res$group.sizes.by.group[[group]], tab.name = paste("Group Sizes -", group))
      }
    }    
  }
  
  if (include.crosstabs) {
    for (country in sort(names(res$crosstabs))) {
      country.sht <- str_replace(country, "Palestinian Territories", "Palestine")
      country.sht <- str_trunc(country.sht, 31)
      
      openxlsx::addWorksheet(wb, country.sht)
      
      startRow <- 1
      for (table.name in group.names) {
        table <- res$crosstabs[[country]][[table.name]]
        
        if (! is.data.frame(table))
          next()
        
        table <- gen.crosstab(table)
        
        headers <- c("Party", attr(table, "group.list"))
        
        openxlsx::writeData(wb, country.sht, table.name, startCol = 1, startRow = startRow, rowNames = F)
        openxlsx::addStyle(wb, sheet = country.sht, hs1, rows = startRow, cols = 1)
        
        table <- table %>% 
          mutate(across(ends_with("percent"), ~.x/100)) %>%
          mutate(across(ends_with("percent"), ~set.class('percentage', .x))) 
        
        headerCol <- 1
        for (header in headers) {
          openxlsx::writeData(wb, country.sht, header, startCol = headerCol, startRow = startRow + 1)
          openxlsx::addStyle(wb, sheet = country.sht, hs2, rows = startRow + 1, cols = headerCol)
          
          if (header != "Party") {
            openxlsx::mergeCells(wb, country.sht, cols = headerCol:(headerCol+1), rows = startRow + 1)
            headerCol <- headerCol+1
          }
          
          headerCol <- headerCol+1
        }
        
        openxlsx::writeData(wb, country.sht, table, startCol = 1, startRow = startRow + 2, rowNames = F, colNames = F)
        openxlsx::setColWidths(wb, sheet = country.sht, cols = 1, widths = "auto")
        openxlsx::setColWidths(wb, sheet = country.sht, cols = 2:ncol(table), widths = "10")
        
        startRow <- nrow(table) + 3 + startRow
      }
      
      openxlsx::writeData(wb, country.sht, "Statistics", startCol = 1, startRow = startRow)
      openxlsx::addStyle(wb, sheet = country.sht, hs1, rows = startRow, cols = 1)
      
      openxlsx::writeData(wb, country.sht, "Sample Size", startCol = 1, startRow = startRow+2)
      openxlsx::writeData(wb, country.sht, res$crosstabs[[country]]$Summary$general$`Sample Size`, startCol = 2, startRow = startRow+2)
      
      openxlsx::writeData(wb, country.sht, "Correlations", startCol = 1, startRow = startRow+4)
      openxlsx::addStyle(wb, sheet = country.sht, hs2, rows = startRow+4, cols = 1)
      openxlsx::writeData(wb, country.sht, res$crosstabs[[country]]$Summary$cor, startCol = 2, startRow = startRow+5)
      
      openxlsx::writeData(wb, country.sht, "Correlations (Party = None/Missing/DK removed)", startCol = 1, startRow = startRow+9)
      openxlsx::addStyle(wb, sheet = country.sht, hs2, rows = startRow+9, cols = 1)
      
      openxlsx::writeData(wb, country.sht, res$crosstabs[[country]]$Summary$cor.nomiss, startCol = 2, startRow = startRow+11)
    }
  }
  
  openxlsx::saveWorkbook(wb, file, overwrite = T)
}

write.excel.summary.tab <- function(wb, summary.data, tab.name = "Summary", included.excluded.col = T) {
  summary.sheet <- summary.data %>%
    mutate(across(ends_with('.pct'), ~set.class('percentage', .))) %>%
    select(-Religion, -Ethnicity, -Language, -ID) %>%
    arrange(desc(PES)) %>%
    select(Country, `Data Source`, Year, `Sample Size`, `Group Basis`, PES, PES.nrm, cor.nomiss, cross.cutting, everything()) %>%
    relocate(warning.flags, .after = everything())
  
  hs2 <- openxlsx::createStyle(textDecoration = "bold")
  
  openxlsx::addWorksheet(wb, tab.name)
  outer.headers <- c("", "", "", "", "", "", "", "", "", "", "",
                     "Included in Group", "", "Party Missing", "", "Group Missing")
  openxlsx::writeData(wb, tab.name, data.frame(t(outer.headers)), startRow = 1, startCol = 1, colNames = F, rowNames = F)
  openxlsx::addStyle(wb, sheet = tab.name, hs2, rows = 1, cols = 1:length(outer.headers))
  openxlsx::mergeCells(wb, tab.name, cols = 12:13, rows = 1)
  openxlsx::mergeCells(wb, tab.name, cols = 14:15, rows = 1)
  openxlsx::mergeCells(wb, tab.name, cols = 16:17, rows = 1)
  
  summary.headers <- c("Country", "Data Source", "Survey Year", "Sample Size", "Group Basis", "PES", "PES.nrm", "Tau", "CC",
                       "PVP", "PVF", "Exclusion Reason", "(N)", "(%)", "(N)", "(%)", "(N)", "(%)", "Warning Flags")
  
  openxlsx::writeData(wb, tab.name, data.frame(t(summary.headers)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  openxlsx::setColWidths(wb, sheet = tab.name, cols = 1:length(summary.headers), widths = "auto")
  openxlsx::addStyle(wb, sheet = tab.name, hs2, rows = 2, cols = 1:length(summary.headers))
  
  openxlsx::writeData(wb, tab.name, summary.sheet, startRow = 3, colNames = F, rowNames = F)
}

write.excel.group.sizes.tab <- function(wb, group.sizes.data, tab.name = "Group Sizes") {
  openxlsx::addWorksheet(wb, tab.name)
  max.parties <- get.max.parties(group.sizes.data)
  
  hs2 <- openxlsx::createStyle(textDecoration = "bold")
  
  openxlsx::writeData(wb, tab.name, "Largest Groups", startRow = 1, startCol = 5)
  
  for (party.num in 1:max.parties) {
    col <- 4 + (summary.group.size * 2) + ( (party.num-1) * (2 + summary.group.size) ) + 1
    openxlsx::writeData(wb, tab.name, paste("Supporters of Party", party.num), startRow = 1, startCol = col)
  }
  
  group.size.names <- c(
    "Country", "Data Source", "Year", "Group Basis",
    rep(c("Name", "N"), summary.group.size),
    rep(c("Party", "Total N", paste("Group", 1:summary.group.size)), max.parties)
  )
  
  openxlsx::writeData(wb, tab.name, data.frame(t(group.size.names)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  openxlsx::writeData(wb, tab.name, group.sizes.data, startRow = 3, startCol = 1, colNames = F, rowNames = F)
  
  header.cols <- 4 + (summary.group.size * 2) + ( (max.parties) * (2 + summary.group.size) )
  openxlsx::setColWidths(wb, sheet = tab.name, cols = 1:header.cols, widths = "auto")
  openxlsx::addStyle(wb, sheet = tab.name, hs2, rows = 1, cols = 1:header.cols)
  openxlsx::addStyle(wb, sheet = tab.name, hs2, rows = 2, cols = 1:header.cols)  
}