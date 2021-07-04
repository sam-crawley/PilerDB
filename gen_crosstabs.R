library(tidyverse)
library(janitor)
library(openxlsx)
library(GoodmanKruskal)
library(here)

source(here("Divided/read_data/wvs.R"))
source(here("Divided/read_data/asian.R"))
source(here("Divided/read_data/afro.R"))

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)

summary.group.size <- 5

# Generate crosstabs for all datasets
gen.all.crosstabs <- function(save.output = F) {
  cat.sum <- list()
  
  # WVS7
  data.wvs7 <- read.data.wvs()
  cat.sum[['WVS7']] <- gen.category.summary(data.wvs7, wvs7.cats)
  overall <- gen.country.crosstabs(data.wvs7, wvs7.cats, "WVS7")

  # Asian Barom 4
  data.asb4 <- read.data.asian()
  cat.sum[['ASB4']] <- gen.category.summary(data.asb4, asb4.cats)
  res <- gen.country.crosstabs(data.asb4, asb4.cats, "ASB4")
  overall <- append(overall, res)
  
  # Afrobarometer 7
  data.afb7 <- read.data.afro()
  cat.sum[['AFB7']] <- gen.category.summary(data.afb7, afro7.cats)
  res <- gen.country.crosstabs(data.afb7, afro7.cats, "AFB7")
  overall <- append(overall, res)
  
  if (save.output) {
    write_rds(overall, "Divided/output/divided.rds")
    write_rds(cat.sum, "Divided/output/divided.category.summary.rds")
  }
  
  overall
}

# Produce a data structure for a single dataset that includes crosstabs for each country
#  At this level, we should only be doing summarising that requires access to the original dataset
#  (Because the idea is that the original data will not be available after this point)
gen.country.crosstabs <- function(data, cat.defs, data.source) {
  # Do common processing of dataset
  data <- process.data(data, cat.defs)
  
  # Create crosstabs for each country
  # (Produces a list of lists, keyed by country+data.source.year)
  res <- map(unique(data$Country), function(cntry) {
    
    d <- data %>% filter(Country == cntry)

    tables <- map(group.names, function(var) {
      if (all( d[var] == "Missing" )) {
        return (NULL)
      }
      
      t <- d %>% 
        tabyl(Party, .data[[var]], show_missing_levels = F)
      t
    })
    
    names(tables) <- group.names
    
    year <- unique(as.numeric(d$Year))
    cor = calc.correlations(d)
    cor.nomiss = calc.correlations(d, cats.to.drop = c("Missing", "Other"))
    
    tables$Summary <- list(
      general = tibble(
        'ID' = paste(cntry, data.source, year),
        'Country' = cntry,
        'Data Source' = data.source,
        'Year' = year,
        'Sample Size' = nrow(d),
        'Group Basis' = cor.nomiss$max.col
      ),
      cor = cor,
      cor.nomiss = cor.nomiss
    )
    
    tables
    
  })
  
  res <- set_names(res, map_chr(res, ~ .x$Summary$general$ID))

  return(res)
}

# Do initial common data processing
process.data <- function(data, cat.defs) {
  for (cat.def.var in names(cat.defs)) {
    for (cat.type in names(cat.defs[[cat.def.var]])){
      new.levels <- list(cat.defs[[cat.def.var]][[cat.type]])
      names(new.levels) <- cat.type
      
      data[[cat.def.var]] <- fct_collapse(data[[cat.def.var]], !!!new.levels)
    }
  }
  
  data
  
}

gen.category.summary <- function(data, cat.defs) {
  map(main.vars, function(var.name) {
    categories <- fct_count(data[[var.name]]) %>%
      mutate("Collapsed To" = case_when(
        f %in% cat.defs[[var.name]]$Missing ~ 'Missing',
        f %in% cat.defs[[var.name]]$Other ~ 'Other',
        TRUE ~ ''
      )) %>%
      rename(
        "Category" = f,
        "N" = n
      )
    
    list(
      question = attr(data[[var.name]], "label"),
      categories = categories
    )
  }) %>% set_names(main.vars)
}

calc.correlations <- function(d, forward = T, cats.to.drop = NULL) {
  drop.cats <- ! is.null(cats.to.drop)
  
  if (drop.cats)
    d <- d %>% filter(! Party %in% cats.to.drop)
  
  tau <- map_dfr(group.names, function(var) {
    d.g <- d 
    
    if (drop.cats)
      d.g <- d.g %>% filter(! .data[[var]] %in% cats.to.drop)
    
    if (nrow(d.g) == 0)
      return(tibble(question = var, assoc = NA))
    
    t <- GKtau(d.g$Party, d.g[[var]])
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
    sum$cor <- orig.sum.data$cor[[orig.sum.data$cor$max.col]]
    sum$cor.nomiss <- orig.sum.data$cor.nomiss[[orig.sum.data$cor.nomiss$max.col]]
    
    main.crosstab <- country.data[[sum$`Group Basis`]]
    
    sum$total.included <- main.crosstab %>% 
      pivot_longer(-Party, names_to = "Group") %>%
      filter(! Party %in% c("Missing", "Other")) %>% 
      filter(! Group %in% c("Missing", "Other")) %>% 
      summarise(v = sum(value)) %>%
      pull(v)
    sum$total.included.pct <- sum$total.included / sum$`Sample Size`
    
    if (main.crosstab %>% filter(Party %in% c("Missing", "Other")) %>% count() > 0) {
      sum$party.missing <- main.crosstab %>% 
        pivot_longer(-Party, names_to = "Group") %>%
        filter(Party %in% c("Missing", "Other")) %>% 
        summarise(v = sum(value)) %>%
        pull(v)
      sum$party.missing.pct <- sum$party.missing / sum$`Sample Size`
    }
    else {
      sum$party.missing <- 0
      sum$party.missing.pct <- 0
    }
    
    if (any(has_name(main.crosstab, c("Missing", "Other")))) {
      sum$group.missing <- main.crosstab %>% 
        pivot_longer(-Party, names_to = "Group") %>%
        filter(Group %in% c("Missing", "Other")) %>% 
        summarise(v = sum(value)) %>%
        pull(v)
      sum$group.missing.pct <- sum$group.missing / sum$`Sample Size`
    }
    else {
      sum$group.missing <- 0
      sum$group.missing.pct <- 0
    }
    
    sum
  })
}

get.group.size.summary <- function(res) {
  # Add in group sizes for 3 largest groups for each country,
  #  as well as breakdowns for each Party/Main Group combo
  map_dfr(res, function(country.data) {
    main.crosstab <- country.data[[country.data$Summary$general$`Group Basis`]]
    
    gs.row <- tibble(
      Country = country.data$Summary$general$Country,
      ID = country.data$Summary$general$ID
    )
    
    gs <- main.crosstab %>% 
      pivot_longer(-Party, names_to = "Group") %>% 
      filter(! Party %in% c("Missing", "Other")) %>% 
      filter(! Group %in% c("Missing", "Other")) %>% 
      group_by(Group) %>% 
      summarise(value = sum(value)) %>% 
      filter(value > 0) %>%
      slice_max(value, n=5, with_ties = F)      

    main.groups <- gs$Group

    for (row in 1:summary.group.size) {
      sum.row <- gs[row, ]
      gs.row <- suppressMessages(bind_cols(gs.row, sum.row))
    }
    
    party.group.sizes <- main.crosstab %>% 
      select(Party, all_of(main.groups)) %>% 
      filter(! Party %in% c("Missing", "Other")) %>% 
      adorn_totals("col") %>% 
      filter(Total >= 20) %>%
      arrange(desc(Total)) %>%
      select(Party, Total, everything())
    
    names(party.group.sizes) <- c("Party.Grp", "Total", paste("Group", 1:length(main.groups)))
    
    # Ensure we always have the right number of groups
    if (length(main.groups) < summary.group.size) {
      for (extra.group in (length(main.groups)+1):summary.group.size) {
        party.group.sizes[[paste('Group', extra.group)]] <- NA
      }
    }
    
    for (row in 1:nrow(party.group.sizes)) {
      gs.row <- suppressMessages(bind_cols(gs.row, party.group.sizes[row, ]))
    }

    gs.row
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
