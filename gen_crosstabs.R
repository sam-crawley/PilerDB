library(tidyverse)
library(janitor)
library(openxlsx)
library(countrycode)
library(haven)
library(GoodmanKruskal)

skip.countries <- c("CHN", "EGY", "VNM", "JOR")

questions <- c("Q272", "Q289", "Q290")
q.names <- c("Language", "Religion", "Ethnicity")
names(q.names) <- questions 
names(questions) <- q.names

summary.group.size <- 3

read.data <- function() {
  data <- read_dta("Divided/data/orig/WVS_Cross-National_Wave_7_stata_v1_6_2.dta", encoding = "UTF-8") %>%
    filter(! B_COUNTRY_ALPHA %in% skip.countries)
  
  data <- data %>%
    mutate(across(c(Q223, Q272, Q289, Q290), haven::as_factor)) %>%
    mutate(across(c(Q223, Q272, Q289, Q290), fct_explicit_na)) %>%
    mutate(Q223 = fct_collapse(Q223,
                               "None/Missing/DK" = c("Not applicable", "No answer", "Don´t know", "No right to vote", "I would not vote", "(Missing)",
                                                     "I would cast a blank ballot; White vote", "None", "Null vote")
    ))
  
  return(data)
}

gen.wvs.crosstabs <- function(write.res = T, lump = T) {
  data <- read.data()
  
  res <- map(unique(data$B_COUNTRY_ALPHA), function(country) {
    
    d <- data %>%
      filter(B_COUNTRY_ALPHA == country)
    
    if (lump) {
      d <- d %>% 
        mutate(across(c(Q223, Q272, Q289, Q290), ~fct_lump_prop(.x, 0.05)))
    }
    
    for (var in c("Q223", "Q272", "Q289", "Q290")) {
      levels(d[[var]]) <- str_remove(levels(d[[var]]), "^\\w+:\\s*")
    }
    
    tables <- map(list("Q272", "Q289", "Q290"), function(var) {
      if (all( d[var] == "(Missing)" )) {
        return (NULL)
      }
      
      setClass <- function(i) { class(i) <- 'percentage'; i} 
      
      t <- d %>% 
        tabyl(Q223, .data[[var]], show_missing_levels = F) %>% 
        adorn_percentages("row") %>% 
        rename("Party" = Q223) %>% 
        mutate(across(-Party, ~setClass(.)))
      
      t <- inner_join(t, attr(t, "core") %>% rename("Party" = Q223), by = "Party", suffix = c(".%", ".n"))
    })
    
    names(tables) <- c("Language", "Relgion", "Ethnicity")
    
    cors <- calc.correlations(d)
    cors.nomiss <- calc.correlations(d, drop.missing = T)
    group.var <- questions[[cors$max.col]]
    missing.counts <- d %>% summarise(
      party.missing.n = sum(Q223 == 'None/Missing/DK'),
      party.missing.pct = sum(Q223 == 'None/Missing/DK') / length(Q223),
      group.missing.n = sum(.data[[group.var]] == 'None/Missing/DK'),
      group.missing.pct = sum(.data[[group.var]] == 'None/Missing/DK') / length(.data[[group.var]]),
      group.party.n = sum(Q223 != 'None/Missing/DK' & .data[[group.var]] != 'None/Missing/DK'),
      group.party.pct = sum(Q223 != 'None/Missing/DK' & .data[[group.var]] != 'None/Missing/DK') / length(Q223),
    )
    
    d.nomiss <- d %>% filter(Q223 != 'None/Missing/DK' & .data[[group.var]] != 'None/Missing/DK')
    
    tables$Summary <- list(
      'Country' = countrycode(country, origin = 'iso3c', destination = 'country.name'),
      'Year' = unique(as.numeric(d$A_YEAR)),
      'Sample Size' = nrow(d),
      'Correlations' = cors,
      'Correlations.nomiss' = cors.nomiss,
      'Missing Counts' = missing.counts,
      'Group Sizes' = fct_count(d.nomiss[[group.var]]) %>% slice_max(n, n = summary.group.size, with_ties = F) %>% filter(n != 0 & f != '(Missing)'),
      'Group Sizes by Party' = d.nomiss %>% group_by(Q223, .data[[group.var]]) %>% count() %>% rename("group" = {{group.var}})
      
    )
    
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

calc.correlations <- function(d, forward = T, drop.missing = F) {
  if (drop.missing)
    d <- d %>% filter(Q223 != "None/Missing/DK")
  
  tau <- map_dfr(unname(questions), function(var) {
    t <- GKtau(d$Q223, d[[var]])
    tibble(question = q.names[[var]], assoc = ifelse(forward, t$tauxy, t$tauyx))
  })

  tau <- tau %>%
    pivot_longer(assoc) %>% 
    pivot_wider(names_from = question) %>%
    select(-name) %>%
    mutate(across(everything(), ~ifelse(is.nan(.x) | .x == -Inf, NA, .x))) %>%
    mutate(max.col = names(.)[which.max(c_across(everything()))])
  
  tau
  
}

write.wvs.xlsx <- function(res) {
  options("openxlsx.numFmt" = NULL)
  wb <- createWorkbook()
  
  hs1 <- createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- createStyle(textDecoration = "bold")
  
  addWorksheet(wb, "Summary")
  summary.headers <- c("Country", "Survey Year", "Sample Size", "Group Basis", "Group Assoc.", "Group Assoc. (missing removed)", 
                       "Incl. Group (N)", "Incl. Group (%)", "Party Missing (N)", "Party Missing (%)", "Group Missing (N)", "Group Missing (%)",
                       "Group 1", "", "Group 2", "", "Group 3", "")
  
  writeData(wb, "Summary", data.frame(t(summary.headers)), startRow = 1, startCol = 1, colNames = F, rowNames = F)
  setColWidths(wb, sheet = "Summary", cols = 1:length(summary.headers), widths = "auto")
  addStyle(wb, sheet = "Summary", hs2, rows = 1, cols = 1:length(summary.headers))
  
  country.count <- 1
  
  max.parties <- 0
  
  for (country in names(res)) {
    addWorksheet(wb, country)
    
    country.count <- country.count + 1
    
    summary.line <- get.country.summary.line(res[[country]])
    
    # Find number of parties included
    party.count <- length(names(summary.line)[str_detect(names(summary.line), "^Party \\d")])
    if (party.count > max.parties)
      max.parties <- party.count
    
    writeData(wb, "Summary", t(summary.line), startRow = country.count, startCol = 1, colNames = F, rowNames = F)
    
    startRow <- 1
    for (table.name in c("Language", "Relgion", "Ethnicity")) {
      table <- res[[country]][[table.name]]
      
      if (is.null(table))
        next()
      
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
    
    writeData(wb, country, res[[country]]$Summary$`Sample Size`, startCol = 1, startRow = startRow+1)
  }
  
  party.headers <- rep(c("Party", "Total N", "Group 1", "Group 2", "Group 3"), max.parties)
  party.headers.start.col <- length(summary.headers)+1
  party.headers.end.col <- party.headers.start.col + length(party.headers)
  
  writeData(wb, "Summary", data.frame(t(party.headers)), startRow = 1, startCol = party.headers.start.col, colNames = F, rowNames = F)
  setColWidths(wb, sheet = "Summary", cols = party.headers.start.col:party.headers.end.col, widths = "auto")
  addStyle(wb, sheet = "Summary", hs2, rows = 1, cols = party.headers.start.col:party.headers.end.col)
  
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
