library(tidyverse)
library(janitor)
library(openxlsx)
library(GoodmanKruskal)
library(here)
library(StatMatch)

source(here("Divided/read_data.R"))

summary.group.size <- 5

# These "countries" should *always* be skipped
global.country.skip <- c("Hong Kong SAR China", "Macao SAR China", "Puerto Rico")

# Generate crosstabs for all datasets
gen.all.crosstabs <- function(ids.to.load = NULL, existing.data = NULL, save.output = F) {
  if (! is.null(ids.to.load) && is.null(existing.data) && save.output)
    stop("Can't save output if ids.to.load provided, but existing.data *not* provided")
  
  tabs <- list()
  cat.sum <- list()
  
  if (! is.null(existing.data)) {
    tabs <- existing.data$crosstabs
    cat.sum <- existing.data$cat.sum
    
    if (! is.null(ids.to.load)) {
      # Remove ids.to.load from existing data
      for (tab in names(tabs)) {
        if (tabs[[tab]]$Summary$general$`Data Source` %in% ids.to.load)
          tabs[[tab]] <- NULL
      }
      
      cat.sum[ids.to.load] <- NULL
    }
  }
  
  data.defs <- list.files(here("Divided/read_data"), pattern="*.R", full.names=T)
  
  for (data.def in data.defs) {
    id <- toupper( str_match(data.def, "/(\\w+?).R$")[,2] )
    
    if (! is.null(ids.to.load)) {
      if (! id %in% ids.to.load)
        next()
    }
    
    cat("Processing", data.def, "\n")
    
    e <- new.env()
    
    source(data.def, local = e)
    
    data <- read.div.data(e$data.spec)
    
    cat.sum[[id]] <- gen.category.summary(data, e$cat.defs)
    src.tabs <- gen.country.crosstabs(data, e$cat.defs, id)
    
    tabs <- append(tabs, src.tabs)
  }
  
  res <- list(
    crosstabs = tabs,
    cat.sum = cat.sum
  )
  
  if (save.output) {
    write_rds(res, "Divided/output/divided.rds")
  }
  
  res
}

# Produce a data structure for a single dataset that includes crosstabs for each country
#  At this level, we should only be doing summarising that requires access to the original dataset
#  (Because the idea is that the original data will not be available after this point)
gen.country.crosstabs <- function(data, cat.defs, data.source) {
  # Do common processing of dataset
  data <- process.data(data, cat.defs)
  
  countries <- data %>%
    filter(! Country %in% global.country.skip) %>%
    pull(Country) %>%
    unique()
  
  # Create crosstabs for each country
  # (Produces a list of lists, keyed by country+data.source.year)
  res <- map(countries, function(cntry) {
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
    cor.wt = calc.correlations(d, use.weights = T)
    cor.nomiss = calc.correlations(d, cats.to.drop = c("Missing", "Other"))
    cor.nomiss.wt = calc.correlations(d, cats.to.drop = c("Missing", "Other"), use.weights = T)
    
    tables$Summary <- list(
      general = tibble(
        'ID' = paste(cntry, data.source, year),
        'Country' = cntry,
        'Data Source' = data.source,
        'Year' = year,
        'Sample Size' = nrow(d),
        'Group Basis' = cor.nomiss$max.col,
        'Gallagher' = calc.gallagher(d, cor.nomiss$max.col),
        'Loosmore Hanby' = calc.gallagher(d, cor.nomiss$max.col, loosemore = T)
      ),
      cor = cor,
      cor.wt = cor.wt,
      cor.nomiss = cor.nomiss,
      cor.nomiss.wt = cor.nomiss.wt
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

calc.correlations <- function(d, cats.to.drop = NULL, use.stat.match = T, use.weights = F) {
  country <- unique(d$Country)
  #cat("Calc correlations for", country, "\n")
  
  drop.cats <- ! is.null(cats.to.drop)
  
  if (drop.cats)
    d <- d %>% filter(! Party %in% cats.to.drop)
  
  tau <- map_dfr(group.names, function(var) {
    d.g <- d 
    
    if (drop.cats)
      d.g <- d.g %>% filter(! .data[[var]] %in% cats.to.drop)
    
    if (nrow(d.g) == 0)
      return(tibble(question = var, assoc = NA))
    
    if (use.stat.match) {
      d.g <- d.g %>% mutate(
        Party = fct_drop(Party),
        "{var}" = fct_drop(.data[[var]])
      )
      
      wt.var = NULL
      if (use.weights)
        wt.var = "Weight"
      
      assoc <- suppressWarnings(pw.assoc(as.formula(paste(var, "~ Party")), d.g, out.df = T, weights = wt.var))
      t <- round(assoc$tau, digits = 3)
    }
    else {
      t <- GKtau(d.g$Party, d.g[[var]])$tauxy  
    }
    
    tibble(question = var, assoc = t)
  })
  
  if (all(is.nan(tau$assoc) | is.na(tau$assoc))) {
    stop("Couldn't calculate any correlations, bad data for ", country, "?")
  }

  tau <- tau %>%
    pivot_longer(assoc) %>% 
    pivot_wider(names_from = question) %>%
    select(-name) %>%
    mutate(across(everything(), ~ifelse(is.nan(.x) | .x == -Inf, NA, .x))) %>%
    mutate(max.col = names(.)[which.max(c_across(everything()))])
  
  tau
}

calc.gallagher <- function(d, group.to.use, max.groups = NULL, loosemore = F) {
  # Remove Missing/Other categories from Party and Grouping vars
  d <- d %>%
    filter(! .data[[group.to.use]] %in% c("Missing", "Other")) %>% 
    filter(! Party %in% c("Missing", "Other"))
  
  grp.sizes <- tabyl(d, {{group.to.use}}, show_missing_levels = F)
  
  # If max.groups is set, find the top n groups, and remove all but these from the data
  if (! is.null(max.groups)) {
    grp.sizes <- grp.sizes %>%
      slice_max(n, n=max.groups, with_ties = F)
    
    d <- d %>% filter(.data[[group.to.use]] %in% grp.sizes[[group.to.use]])
    
    # Recalculate grp.sizes table so that percentages are right
    grp.sizes <- tabyl(d, {{group.to.use}}, show_missing_levels = F)
  }
  
  grp.sizes <- grp.sizes %>%
    mutate(percent = percent*100)

  # Create lookup of group sizes
  grp.size.list <- map(grp.sizes[[group.to.use]], ~grp.sizes %>% filter(.data[[group.to.use]] == .x) %>% pull(percent)) %>%
    set_names(grp.sizes[[group.to.use]])
  
  party.sizes.by.grp <- tabyl(d, Party, .data[[group.to.use]], show_missing_levels = F) %>% adorn_percentages()
  
  # Calculate unweighted values for each party
  res <- party.sizes.by.grp %>% 
    pivot_longer(-Party) %>% 
    mutate(value = unlist(grp.size.list[name]) - as.numeric(value)*100)
  
  if (loosemore) {
    res <- res %>% 
      mutate(value = abs(value)) %>% 
      group_by(Party) %>% 
      summarise(total = sum(value)/2)
  }
  else {
    res <- res %>% 
      mutate(value = value*value) %>% 
      group_by(Party) %>% 
      summarise(total = sqrt(sum(value)/2))
  }
  
  # Apply weights
  res.wt <- d %>%
     group_by(Party) %>%
     count(Party) %>%
     mutate(weight = n / nrow(d)) %>%
     inner_join(res, by = "Party") %>%
     mutate(total = total * weight)
  
 res.wt %>% 
   ungroup() %>% 
   summarise(total = sum(total)) %>%
   pull(total)
  
}

# Calculate a DF summarising all countries
calc.summary.data <- function(res) {
  map_dfr(res, function(country.data) {
    orig.sum.data <- country.data$Summary
    #cat(orig.sum.data$general$Country, "\n")
    
    sum <- orig.sum.data$general
    sum$Gallagher <- round(sum$Gallagher, 2)
    sum$`Loosmore Hanby` <- round(sum$`Loosmore Hanby`, 2)
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
    
    gs.row <- country.data$Summary$general %>%
      select(-ID, -`Sample Size`)
    
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

get.max.parties <- function(group.sizes) {
  max.parties <- length(names(group.sizes)[str_detect(names(group.sizes), "^Party.Grp")])
}



write.wvs.xlsx <- function(res, file = "Divided/output/divided_crosstabs.xlsx") {
  summary.sheet <- get.excel.summary.sheet(res) %>%
    select(-ID) %>%
    arrange(desc(cor))
  
  options("openxlsx.numFmt" = NULL)
  wb <- createWorkbook()
  
  hs1 <- createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- createStyle(textDecoration = "bold")
  
  # Add summary sheet with 2 header rows
  addWorksheet(wb, "Summary")
  outer.headers <- c("", "", "", "", "", "Highest Group Correlation", "",
                     "Included in Group", "", "Party Missing", "", "Group Missing")
  writeData(wb, "Summary", data.frame(t(outer.headers)), startRow = 1, startCol = 1, colNames = F, rowNames = F)
  addStyle(wb, sheet = "Summary", hs2, rows = 1, cols = 1:length(outer.headers))
  mergeCells(wb, "Summary", cols = 6:7, rows = 1)
  mergeCells(wb, "Summary", cols = 8:9, rows = 1)
  mergeCells(wb, "Summary", cols = 10:11, rows = 1)
  
  summary.headers <- c("Country", "Data Source", "Survey Year", "Sample Size", "Group Basis", "(full sample)", "(Missing/Other removed)", 
                       "(N)", "(%)", "(N)", "(%)", "(N)", "(%)")
  
  writeData(wb, "Summary", data.frame(t(summary.headers)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  setColWidths(wb, sheet = "Summary", cols = 1:length(summary.headers), widths = "auto")
  addStyle(wb, sheet = "Summary", hs2, rows = 2, cols = 1:length(summary.headers))
  
  writeData(wb, "Summary", summary.sheet, startRow = 3, colNames = F, rowNames = F)
  
  # Add Group Sizes sheet
  addWorksheet(wb, "Group Sizes")
  group.sizes <- get.group.size.summary(res)
  max.parties <- get.max.parties(group.sizes)
  
  writeData(wb, "Group Sizes", "Largest Groups", startRow = 1, startCol = 5)
  
  for (party.num in 1:max.parties) {
    col <- 4 + (summary.group.size * 2) + ( (party.num-1) * (2 + summary.group.size) ) + 1
    writeData(wb, "Group Sizes", paste("Supporters of Party", party.num), startRow = 1, startCol = col)
  }

  group.size.names <- c(
    "Country", "Data Source", "Year", "Group Basis",
    rep(c("Name", "N"), summary.group.size),
    rep(c("Party", "Total N", paste("Group", 1:summary.group.size)), max.parties)
  )
    
  writeData(wb, "Group Sizes", data.frame(t(group.size.names)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  writeData(wb, "Group Sizes", group.sizes, startRow = 3, startCol = 1, colNames = F, rowNames = F)
  
  header.cols <- 4 + (summary.group.size * 2) + ( (max.parties) * (2 + summary.group.size) )
  setColWidths(wb, sheet = "Group Sizes", cols = 1:header.cols, widths = "auto")
  addStyle(wb, sheet = "Group Sizes", hs2, rows = 1, cols = 1:header.cols)
  addStyle(wb, sheet = "Group Sizes", hs2, rows = 2, cols = 1:header.cols)
  
  for (country in sort(names(res))) {
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
  
  saveWorkbook(wb, file, overwrite = T)
}
