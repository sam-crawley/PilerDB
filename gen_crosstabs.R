library(tidyverse)
library(janitor)
library(openxlsx)
library(GoodmanKruskal)

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)

summary.group.size <- 3

# Generate crosstabs for all datasets
gen.all.crosstabs <- function(save.output = F) {
  # WVS7
  data.wvs7 <- read.data.wvs()
  overall <- gen.country.crosstabs(data.wvs7, "WVS7")

  # Asian Barom 4
  data.asb4 <- read.data.asian()
  res <- gen.country.crosstabs(data.asb4, "ASB4")
  overall <- append(overall, res)
  
  
  if (save.output)
    write_rds(overall, "Divided/output/divided.rds")
  
  overall
}

# Produce a data structure for a single dataset that includes crosstabs for each country
#  At this level, we should only be doing summarising that requires access to the original dataset
#  (Because the idea is that the original data will not be available after this point)
gen.country.crosstabs <- function(data, data.source) {
  # Create crosstabs for each country
  # (Produces a list of lists, keyed by country+year)
  res <- map(unique(data$Country), function(cntry) {
    
    d <- data %>% filter(Country == cntry)

    tables <- map(group.names, function(var) {
      if (all( d[var] == "(Missing)" )) {
        return (NULL)
      }
      
      t <- d %>% 
        tabyl(Party, .data[[var]], show_missing_levels = F)
      t
    })
    
    names(tables) <- group.names
    
    tables$Summary <- list(
      general = tibble(
        'Country' = cntry,
        'Data Source' = data.source,
        'Year' = unique(as.numeric(d$Year)),
        'Sample Size' = nrow(d)
      ),
      cor = calc.correlations(d),
      cor.nomiss = calc.correlations(d, drop.missing = T)
    )
    
    tables
    
  }) %>%
    set_names(
      data %>% select(Country, Year) %>% distinct() %>% 
        mutate(Tab.Name = paste(Country, Year, data.source)) %>%
        pull(Tab.Name)
    )

  return(res)
}

calc.correlations <- function(d, forward = T, drop.missing = F) {
  if (drop.missing)
    d <- d %>% filter(Party != "None/Missing/DK")
  
  tau <- map_dfr(group.names, function(var) {
    t <- GKtau(d$Party, d[[var]])
    tibble(question = var, assoc = ifelse(forward, t$tauxy, t$tauyx))
  })

  tau <- tau %>%
    pivot_longer(assoc) %>% 
    pivot_wider(names_from = question) %>%
    select(-name) %>%
    mutate(across(everything(), ~ifelse(is.nan(.x) | .x == -Inf, NA, .x))) %>%
    mutate(max.col = names(.)[which.max(c_across(everything()))])
  
  tau
}

# Calculate a DF summarising all countries
calc.summary.data <- function(res) {
  map_dfr(res, function(country.data) {
    orig.sum.data <- country.data$Summary
    #cat(orig.sum.data$general$Country, "\n")
    
    sum <- orig.sum.data$general
    sum$group.basis <- orig.sum.data$cor$max.col
    sum$cor <- orig.sum.data$cor[[orig.sum.data$cor$max.col]]
    sum$cor.nomiss <- orig.sum.data$cor[[orig.sum.data$cor.nomiss$max.col]]
    
    main.crosstab <- country.data[[sum$group.basis]]
    
    sum$total.included <- main.crosstab %>% 
      filter(Party != "None/Missing/DK") %>% 
      adorn_totals(where = c("row", "col")) %>%
      filter(Party == "Total") %>%
      pull(Total)
    sum$total.included.pct <- sum$total.included / sum$`Sample Size`
    
    if (main.crosstab %>% filter(Party == "None/Missing/DK") %>% count() == 1) {
      sum$party.missing <- main.crosstab %>% 
        adorn_totals(where = c("row", "col")) %>%
        filter(Party == "None/Missing/DK") %>%
        pull(Total)
      sum$party.missing.pct <- sum$party.missing / sum$`Sample Size`
    }
    else {
      sum$party.missing <- 0
      sum$party.missing.pct <- 0
    }
    
    if (has_name(main.crosstab, "(Missing)")) {
      sum$group.missing <- main.crosstab %>% 
        adorn_totals(where = "row") %>%
        filter(Party == "Total") %>%
        pull(.data[["(Missing)"]])
      sum$group.missing.pct <- sum$group.missing / sum$`Sample Size`
    }
    else {
      sum$group.missing <- 0
      sum$group.missing.pct <- 0
    }
    
    sum
  })
}

get.country.data <- function(res, row) {
  res[[as.numeric(row)]]
}

set.class <- function(class.name, i) { class(i) <- class.name; i} 

reformat.table.for.excel <- function(table) {
  table <- table %>% 
    adorn_percentages("row") %>% 
    mutate(across(-Party, ~set.class('percentage', .)))

  inner_join(table, attr(table, "core"), by = "Party", suffix = c(".%", ".n"))
}

get.excel.summary.sheet <- function(res) {
  summary.sheet <- calc.summary.data(res) %>%
    mutate(across(ends_with('.pct'), ~set.class('percentage', .)))
  
  # Add in group sizes for 3 largest groups for each country,
  #  as well as breakdowns for each Party/Main Group combo
  group.sizes <- map_dfr(res, function(country.data) {
    main.crosstab <- country.data[[country.data$Summary$cor$max.col]]
    
    gs <- main.crosstab %>% 
      select(-contains("(Missing)")) %>%
      filter(Party != "None/Missing/DK") %>%
      adorn_totals(where = "row") %>% 
      filter(Party == "Total") %>% 
      pivot_longer(-Party) %>% 
      slice_max(value, n = 3, with_ties = F) %>%
      select(-Party)
    
    main.groups <- gs$name
    gs.row <- gs[1, ]
    
    for (row in 2:summary.group.size) {
      gs.row <- suppressMessages(bind_cols(gs.row, gs[row, ]))
    }
    
    party.group.sizes <- main.crosstab %>% 
      select(Party, all_of(main.groups)) %>% 
      filter(Party != "None/Missing/DK") %>% 
      adorn_totals("col") %>% 
      filter(Total >= 20) %>%
      arrange(desc(Total)) %>%
      select(Party, Total, everything())
    
    names(party.group.sizes) <- c("Party.Grp", "Total", paste("Group", 1:length(main.groups)))
    
    # Ensure we always have 3 groups
    if (length(main.groups) < summary.group.size) {
      for (extra.group in (length(main.groups)+1):summary.group.size) {
        party.group.sizes[[paste('Group ', extra.group)]] <- NA
      }
    }
    
    for (row in 1:nrow(party.group.sizes)) {
      gs.row <- suppressMessages(bind_cols(gs.row, party.group.sizes[row, ]))
    }
    
    gs.row
  })
  
  summary.sheet <- suppressMessages(bind_cols(summary.sheet, group.sizes))
  
  return(summary.sheet)
}

write.wvs.xlsx <- function(res) {
  summary.sheet <- get.excel.summary.sheet(res) %>%
    arrange(desc(cor))
  
  options("openxlsx.numFmt" = NULL)
  wb <- createWorkbook()
  
  hs1 <- createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- createStyle(textDecoration = "bold")
  
  # Add summary sheet with 2 header rows
  addWorksheet(wb, "Summary")
  outer.headers <- c("", "", "", "", "Highest Group Correlation", "",
                     "Included in Group", "", "Party Missing", "", "Group Missing", "",
                     "Group Sizes")
  writeData(wb, "Summary", data.frame(t(outer.headers)), startRow = 1, startCol = 1, colNames = F, rowNames = F)
  addStyle(wb, sheet = "Summary", hs2, rows = 1, cols = 1:length(outer.headers))
  mergeCells(wb, "Summary", cols = 5:6, rows = 1)
  mergeCells(wb, "Summary", cols = 7:8, rows = 1)
  mergeCells(wb, "Summary", cols = 9:10, rows = 1)
  
  summary.headers <- c("Country", "Data Source", "Survey Year", "Sample Size", "Group Basis", "(All categories)", "(Missing removed)", 
                       "(N)", "(%)", "(N)", "(%)", "(N)", "(%)",
                       "Group 1", "", "Group 2", "", "Group 3", "")
  
  writeData(wb, "Summary", data.frame(t(summary.headers)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  setColWidths(wb, sheet = "Summary", cols = 1:length(summary.headers), widths = "auto")
  addStyle(wb, sheet = "Summary", hs2, rows = 2, cols = 1:length(summary.headers))
  
  writeData(wb, "Summary", summary.sheet, startRow = 3, colNames = F, rowNames = F)
  
  for (country in names(res)) {
    addWorksheet(wb, country)
    
    startRow <- 1
    for (table.name in group.names) {
      table <- res[[country]][[table.name]]
      
      if (is.null(table))
        next()
      
      writeData(wb, country, table.name, startCol = 1, startRow = startRow, rowNames = F)
      addStyle(wb, sheet = country, hs1, rows = startRow, cols = 1)
      
      table <- reformat.table.for.excel(table)
      
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
    
    writeData(wb, country, "Statistics", startCol = 1, startRow = startRow)
    addStyle(wb, sheet = country, hs1, rows = startRow, cols = 1)
    
    writeData(wb, country, "Sample Size", startCol = 1, startRow = startRow+2)
    writeData(wb, country, res[[country]]$Summary$general$`Sample Size`, startCol = 2, startRow = startRow+2)
    
    writeData(wb, country, "Correlations", startCol = 1, startRow = startRow+4)
    addStyle(wb, sheet = country, hs2, rows = startRow+4, cols = 1)
    writeData(wb, country, res[[country]]$Summary$cor %>% select(-max.col), startCol = 2, startRow = startRow+5)
    
    writeData(wb, country, "Correlations (Party = None/Missing/DK removed)", startCol = 1, startRow = startRow+9)
    addStyle(wb, sheet = country, hs2, rows = startRow+9, cols = 1)
    
    writeData(wb, country, res[[country]]$Summary$cor.nomiss %>% select(-max.col), startCol = 2, startRow = startRow+11)
  }
  
  max.parties <- length(names(summary.sheet)[str_detect(names(summary.sheet), "^Party.Grp")])
  
  party.headers.single <- c("Party", "Total N", "Group 1", "Group 2", "Group 3")
  party.headers <- rep(party.headers.single, max.parties)
  party.headers.start.col <- length(summary.headers)+1
  party.headers.end.col <- party.headers.start.col + length(party.headers)
   
  outer.party.headers <- paste("Supporters of Party", 1:max.parties)
  for (party.num in 1:max.parties) {
    col <- party.headers.start.col + ( (party.num-1) * length(party.headers.single) )
    writeData(wb, "Summary", paste("Supporters of Party", party.num), startRow = 1, startCol = col)
  }
   
  writeData(wb, "Summary", data.frame(t(party.headers)), startRow = 2, startCol = party.headers.start.col, colNames = F, rowNames = F)
  setColWidths(wb, sheet = "Summary", cols = party.headers.start.col:party.headers.end.col, widths = "auto")
   
  addStyle(wb, sheet = "Summary", hs2, rows = 1, cols = party.headers.start.col:party.headers.end.col)
  addStyle(wb, sheet = "Summary", hs2, rows = 2, cols = party.headers.start.col:party.headers.end.col)
  
  saveWorkbook(wb, "Divided/data/output/divided_crosstabs.xlsx", overwrite = T)
}
