library(tidyverse)
library(janitor)
library(openxlsx)
library(countrycode)
library(haven)
library(GoodmanKruskal)

wvs7.skip.countries <- c("CHN", "EGY", "VNM", "JOR")

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)

summary.group.size <- 3

read.data <- function() {
  data <- read_dta("Divided/data/orig/WVS_Cross-National_Wave_7_stata_v1_6_2.dta", encoding = "UTF-8") %>%
    filter(! B_COUNTRY_ALPHA %in% wvs7.skip.countries)
  
  data <- data %>%
    mutate(across(c(Q223, Q272, Q289, Q290), haven::as_factor)) %>%
    mutate(across(c(Q223, Q272, Q289, Q290), fct_explicit_na)) %>%
    mutate(Q223 = fct_collapse(Q223,
                               "None/Missing/DK" = c("Not applicable", "No answer", "Don´t know", "No right to vote", "I would not vote", "(Missing)",
                                                     "I would cast a blank ballot; White vote", "None", "Null vote")
    )) %>%
    rename(
      "Party" = Q223,
      "Language" = Q272,
      "Religion" = Q289,
      "Ethnicity" = Q290,
      "Country.Code" = B_COUNTRY_ALPHA
    ) %>%
    mutate("Country" = countrycode(Country.Code, origin = 'iso3c', destination = 'country.name')) 
  
  # Strip out country prefixes from levels
  for (var in main.vars) {
    levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\w+:\\s*")
  }
  
  return(data)
}

# Produce a summary data structure
#  At this level, we should only be doing summarising that requires access to the original dataset
#  (Because the idea is that the original data will not be available after this point)
gen.wvs.crosstabs <- function(data = NULL, lump = F) {
  if (is.null(data))
    data <- read.data()
  
  # Create crosstabs for each country
  # (Produces a list of lists, keyed by country+year)
  res <- map(unique(data$Country), function(cntry) {
    
    d <- data %>% filter(Country == cntry)
    
    if (lump) {
      d <- d %>% 
        mutate(across(all_of(main.vars), ~fct_lump_prop(.x, 0.05)))
    }

    tables <- map(group.names, function(var) {
      if (all( d[var] == "(Missing)" )) {
        return (NULL)
      }
      
      t <- d %>% 
        tabyl(Party, .data[[var]], show_missing_levels = F) #%>% 
      t
    })
    
    names(tables) <- group.names
    
    tables$Summary <- list(
      general = tibble(
        'Country' = cntry,
        'Year' = unique(as.numeric(d$A_YEAR)), # Fix me
        'Sample Size' = nrow(d)
      ),
      cor = calc.correlations(d),
      cor.nomiss = calc.correlations(d, drop.missing = T)
    )
    
    tables
    
  }) %>%
    set_names(
      data %>% select(Country, A_YEAR) %>% distinct() %>% 
        unite("Tab.Name", Country, A_YEAR, sep = " ") %>%
        pull(Tab.Name)
    )
  
  
  # TODO: change cross tabs to be 'raw' so that these can easily be generated
    #d.nomiss <- d %>% filter(Party != 'None/Missing/DK' & .data[[group.var]] != '(Missing)')
    #d <- data %>% filter(Country == cntry)
    
      #'Missing Counts' = missing.counts,
      #'Group Sizes' = fct_count(d.nomiss[[group.var]]) %>% slice_max(n, n = summary.group.size, with_ties = F) %>% filter(n != 0 & f != '(Missing)'),
      #'Group Sizes by Party' = d.nomiss %>% group_by(Party, .data[[group.var]]) %>% count() %>% rename("group" = {{group.var}})

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
  
  # Add in group sizes for 3 largest groups for each country
  group.sizes <- map_dfr(res, function(country.data) {
    main.crosstab <- country.data[[country.data$Summary$cor$max.col]]
    
    gs <- main.crosstab %>% 
      select(-contains("(Missing)")) %>%
      adorn_totals(where = "row") %>% 
      filter(Party == "Total") %>% 
      pivot_longer(-Party) %>% 
      slice_max(value, n = 3) %>%
      select(-Party)
    
    gs.row <- gs[1, ]
    
    for (row in 2:summary.group.size) {
      gs.row <- suppressMessages(bind_cols(gs.row, gs[row, ]))
    }
    
    gs.row
  })
  
  summary.sheet <- suppressMessages(bind_cols(summary.sheet, group.sizes))
  
  return(summary.sheet)
}

write.wvs.xlsx <- function(res) {
  summary.data <- get.excel.summary.sheet(res)
  
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
  
  summary.headers <- c("Country", "Survey Year", "Sample Size", "Group Basis", "(All categories)", "(Missing removed)", 
                       "(N)", "(%)", "(N)", "(%)", "(N)", "(%)",
                       "Group 1", "", "Group 2", "", "Group 3", "")
  
  writeData(wb, "Summary", data.frame(t(summary.headers)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  setColWidths(wb, sheet = "Summary", cols = 1:length(summary.headers), widths = "auto")
  addStyle(wb, sheet = "Summary", hs2, rows = 2, cols = 1:length(summary.headers))
  
  writeData(wb, "Summary", summary.data, startRow = 3, colNames = F, rowNames = F)
  
  #max.parties <- 0
  
  for (country in names(res)) {
    addWorksheet(wb, country)
    
    # Find number of parties included
    #party.count <- length(names(summary.line)[str_detect(names(summary.line), "^Party \\d")])
    #if (party.count > max.parties)
    #  max.parties <- party.count
    
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
    
    writeData(wb, country, "Sample Size", startCol = 1, startRow = startRow)
    addStyle(wb, sheet = country, hs1, rows = startRow, cols = 1)
    
    writeData(wb, country, res[[country]]$Summary$general$`Sample Size`, startCol = 1, startRow = startRow+1)
  }
  
  # party.headers.single <- c("Party", "Total N", "Group 1", "Group 2", "Group 3")
  # party.headers <- rep(party.headers.single, max.parties)
  # party.headers.start.col <- length(summary.headers)+1
  # party.headers.end.col <- party.headers.start.col + length(party.headers)
  # 
  # outer.party.headers <- paste("Supporters of Party", 1:max.parties)
  # for (party.num in 1:max.parties) {
  #   col <- party.headers.start.col + ( (party.num-1) * length(party.headers.single) )
  #   writeData(wb, "Summary", paste("Supporters of Party", party.num), startRow = 1, startCol = col)
  # }
  # 
  # writeData(wb, "Summary", data.frame(t(party.headers)), startRow = 2, startCol = party.headers.start.col, colNames = F, rowNames = F)
  # setColWidths(wb, sheet = "Summary", cols = party.headers.start.col:party.headers.end.col, widths = "auto")
  # 
  # addStyle(wb, sheet = "Summary", hs2, rows = 1, cols = party.headers.start.col:party.headers.end.col)
  # addStyle(wb, sheet = "Summary", hs2, rows = 2, cols = party.headers.start.col:party.headers.end.col)
  
  saveWorkbook(wb, "Divided/data/output/wvs_crosstabs.xlsx", overwrite = T)
  
}

# Get line for each country that appears on the summary sheet
get.country.summary.line <- function(country.data) {
  summary.data <- country.data[['Summary']]
  
  sum.line <- summary.data[c('Country', 'Year', 'Sample Size')]
  sum.line$`Group Basis` <- summary.data$Correlations$max.col
  
  max.col <- summary.data$Correlations$max.col
  
  if (is.na(max.col)) {
    stop("Couldn't find max col for country ", sum.line$Country)
  }
  
  sum.line$`Group Assoc` <- summary.data$Correlations %>% pull({{max.col}})
  sum.line$`Group Assoc.nomiss` <- summary.data$Correlations.nomiss %>% pull({{max.col}})
  
  sum.line$`PartGrp` <- summary.data$`Missing Counts`$group.party.n
  sum.line$`PartGrp %` <- round(summary.data$`Missing Counts`$group.party.pct, 2) * 100
  sum.line$`Missing Party` <- summary.data$`Missing Counts`$party.missing.n
  sum.line$`Missing Party %` <- round(summary.data$`Missing Counts`$party.missing.pct, 2) * 100
  sum.line$`Missing Group` <- summary.data$`Missing Counts`$group.missing.n
  sum.line$`Missing Group %` <- round(summary.data$`Missing Counts`$group.missing.pct, 2) * 100
  
  for (row in 1:summary.group.size) {
    if (nrow(summary.data$`Group Sizes`) < row) {
      sum.line[[paste0("Group name",row)]] <- ""
      sum.line[[paste0("Group size",row)]] <- ""
      next()
    }
    
    gn <- summary.data$`Group Sizes`[row, "f"][[1]]
    sum.line[[paste0("Group name",row)]] <- levels(gn)[gn]
    sum.line[[paste0("Group size",row)]] <- summary.data$`Group Sizes`[row, "n"]
  }
  
  groups <- summary.data$`Group Sizes`$f %>% as.character()
  party.sizes <- summary.data$`Group Sizes by Party` %>% 
    group_by(Q223) %>% 
    summarise(party.size = sum(n)) %>% 
    filter(party.size >= 20) %>%
    arrange(desc(party.size))
  
  parties <- party.sizes %>% 
    pull(Q223) %>% 
    as.character()
  
  party.count <- 0
  for (party.name in parties) {
    party.count <- party.count + 1
    
    sum.line[[paste("Party", party.count)]] <- party.name
    sum.line[[paste("Party N", party.count)]] <- party.sizes %>% filter(Q223 == party.name) %>% pull(party.size)
    
    for (group.count in 1:summary.group.size) {
      if (length(groups) < group.count) {
        sum.line[[paste("PartyGroup", party.count, group.count)]] <- ""
        next()
      }
      
      row <- summary.data$`Group Sizes by Party` %>% filter(Q223 == party.name & group == groups[[group.count]])

      sum.line[[paste("PartyGroup", party.count, group.count)]] <- ifelse(nrow(row) == 0, 0, row$n)
    }
  }
  
  unlist(sum.line)
}
