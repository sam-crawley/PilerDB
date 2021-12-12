library(tidyverse)
library(janitor)
library(openxlsx)
library(here)
library(StatMatch)
library(rlist)
library(stringi)

source(here("Divided/read_data.R"))

summary.group.size <- 5

# These "countries" should *always* be skipped
global.country.skip <- c("Hong Kong SAR China", "Macao SAR China", "Puerto Rico")

cats.to.drop <- c("Missing", "Other")

# Generate crosstabs for all datasets
gen.all.crosstabs <- function(ids.to.load = NULL, existing.data = NULL, save.output = F, calc.summaries = T) {
  if (! is.null(ids.to.load) && is.null(existing.data) && save.output)
    stop("Can't save output if ids.to.load provided, but existing.data *not* provided")
  
  tabs <- list()
  cat.sum <- list()
  data.src.info <- list()
  
  if (! is.null(existing.data)) {
    tabs <- existing.data$crosstabs
    cat.sum <- existing.data$cat.sum
    data.src.info <- existing.data$data.src.info
    
    if (! is.null(ids.to.load)) {
      # Remove ids.to.load from existing data
      for (tab in names(tabs)) {
        if (tabs[[tab]]$Summary$general$`Data Source Orig` %in% ids.to.load)
          tabs[[tab]] <- NULL
      }
      
      cat.sum[ids.to.load] <- NULL
      data.src.info[ids.to.load] <- NULL
    }
  }
  
  data.defs <- get.data.def.list()
  
  for (data.def in data.defs) {
    id <- get.data.def.id(data.def)
    
    if (! is.null(ids.to.load)) {
      if (! id %in% ids.to.load)
        next()
    }
    
    cat("Processing", data.def, "\n")
    
    e <- new.env()
    
    source(data.def, local = e, encoding = "UTF-8")
    
    data <- read.div.data(e$data.spec)
    
    cat.sum[[id]] <- gen.category.summary(data, e$cat.defs)
    src.tabs <- gen.country.crosstabs(data, e$cat.defs, id, wave.var = e$data.spec$wave_var, split.by.year = e$data.spec$split.by.year)
    data.src.info[[id]] <- get.data.src.info(data, e$data.spec)
    
    tabs <- append(tabs, src.tabs)
  }
  
  res <- list(
    crosstabs = tabs,
    cat.sum = cat.sum,
    data.src.info = data.src.info
  )
  
  if (calc.summaries) {
    res$summary <- calc.summary.data(res$crosstabs)
    res$group.sizes <- get.group.size.summary(res$crosstabs)
    res$max.parties <- get.max.parties(res$group.sizes)
  }
  
  if (save.output) {
    write_rds(res, "Divided/output/divided.rds")
  }
  
  res
}

# Produce a data structure for a single dataset that includes crosstabs for each country
#  At this level, we should only be doing summarising that requires access to the original dataset
#  (Because the idea is that the original data will not be available after this point)
gen.country.crosstabs <- function(data, cat.defs, data.source, wave.var = NULL, split.by.year = F) {
  if (is.null(split.by.year))
    split.by.year = F
  
  # Do common processing of dataset
  data <- process.data(data, cat.defs)
  
  data <- data %>%
    filter(! Country %in% global.country.skip)
  
  # Split data up by country
  split.factor <- list(data$Country)
  if (! is.null(wave.var)) {
    split.factor <- list.append(split.factor, data[[wave.var]])
  }
  if (split.by.year) {
    split.factor <- list.append(split.factor, data$Year)
  }
  
  data.by.country <- split(data, split.factor, drop = T)
  
  # Create crosstabs for each country
  # (Produces a list of lists, keyed by country+data.source+year)
  res <- map(names(data.by.country), function(key) {
    cntry <- key
    data.source.orig = data.source
    year <- NULL
    
    splt <- str_split(key, "\\.", simplify = T)
    cntry <- splt[[1]]
    
    splt.idx <- 2
    if (! is.null(wave.var)) {
      data.source <- paste0(data.source, splt[[splt.idx]])
      splt.idx <- splt.idx+1
    }
    if (split.by.year) {
      year <- as.integer(splt[[splt.idx]])
    }
    
    gen.single.country.data(data.by.country[[key]], cntry, data.source, data.source.orig, year)
  })
  
  res <- set_names(res, map_chr(res, ~ .x$Summary$general$ID))

  return(res)
}

gen.single.country.data <- function(d, cntry, data.source, data.source.orig, year = NULL) {
  
  country.orig <- d %>% distinct(Country.orig) %>% pull(Country.orig)
  
  tables <- map(group.names, function(var) {
    calc.summarised.group.data(d, var)
  })
  
  names(tables) <- group.names
  
  if (is.null(year))
    year <- max(as.numeric(d$Year), na.rm = T)
  
  cor = calc.correlations(d)
  cor.wt = calc.correlations(d, use.weights = T)
  cor.nomiss = calc.correlations(d, drop.cats = T)
  cor.nomiss.wt = calc.correlations(d, drop.cats = T, use.weights = T)
  
  group.basis <- calc.group.basis(cor.nomiss)
  
  gallagher <- NA
  pvp <- NA
  pvf <- NA
  
  if (! is.na(group.basis)) {
    cor.vals <- cor.nomiss %>% filter(question == group.basis)
    gallagher <- cor.vals$gallagher
    pvp <- cor.vals$PVP
    pvf <- cor.vals$PVF
  }
  
  tables$Summary <- list(
    general = tibble(
      'ID' = paste(cntry, data.source, year),
      'Country' = cntry,
      'Data Source' = data.source,
      'Data Source Orig' = data.source.orig,
      'Year' = year,
      'Sample Size' = nrow(d),
      'Group Basis' = group.basis,
      'Gallagher' = gallagher,
      'Loosmore Hanby' = calc.gallagher(d, group.basis, loosemore = T),
      'PVP' = pvp,
      'PVF' = pvf
    ),
    cor = cor,
    cor.wt = cor.wt,
    cor.nomiss = cor.nomiss,
    cor.nomiss.wt = cor.nomiss.wt,
    country.orig = country.orig
    
  )
  
  tables
}

# Calc summarised group data for a single country
#  This data is stored in the .rds file and can be converted into crosstabs
#  for display purposes
calc.summarised.group.data <- function(data, group.var) {
  if (all( data[group.var] == "Missing" )) {
    return (NULL)
  }
  
  data %>%
    group_by(Party, .data[[group.var]]) %>%
    summarise(
      n = n(),
      n.weighted = round(sum(Weight), 0),
      .groups = "drop"
    ) %>%
    rename(
      Group = all_of(group.var)
    ) %>%
    mutate(
      Party = fct_drop(Party),
      Group = fct_drop(Group)
    ) %>%
    complete(Party, Group, fill = list(n = 0, n.weighted = 0))
}

# Helper function to find the groups / parties smaller than 0.02
#  to drop based on group summary data
find.groups.to.drop <- function(summary.data, group.type) {
  summary.data %>%
    group_by(.data[[group.type]]) %>% 
    summarise(n = sum(n), .groups = "drop") %>% 
    mutate(percent = n / sum(n)) %>% 
    filter(percent <= 0.02) %>% 
    pull(.data[[group.type]]) %>%
    as.character()
}

# "Configure" the summary.data DF, dropping categories as necessary,
#  and selecting the 'n' column (weighted or unweighted)
config.summary.data <- function(summary.data, drop.cats = F, weighted = F) {
  # Select the right n column, depending on whether we wanted weighted or
  #  unweighted
  if (weighted) {
    summary.data <- summary.data %>% 
      select(-n) %>%
      rename(n = "n.weighted")
  }  
  else {
    summary.data <- summary.data %>% 
      select(-n.weighted)
  }  
  
  # Drop out unwanted categories, and those < 0.02
  if (drop.cats) {
    parties.to.drop <- find.groups.to.drop(summary.data, "Party")
    groups.to.drop  <- find.groups.to.drop(summary.data, "Group")
    
    summary.data <- summary.data %>%
      filter(! Party %in% c(cats.to.drop, parties.to.drop)) %>%
      filter(! Group %in% c(cats.to.drop, groups.to.drop))
  }
  
  summary.data
  
}

# Generate a single crosstab from stored summary data
gen.crosstab <- function(summary.data, drop.cats = F, weighted = F, totals = F) {
  if (is.null(summary.data))
    return (NULL)
  
  summary.data <- config.summary.data(summary.data, drop.cats = drop.cats, weighted = weighted)
  
  to.pct <- function(n, t = NULL) {
    if (is.null(t))
      t <- sum(n)
    
    round(n / t * 100, 1)
  }
  
  sample.size <- sum(summary.data$n)
  
  if (totals) {
    group.totals <- summary.data %>% 
      group_by(Group) %>% 
      summarise(n = sum(n), .groups = "drop") %>% 
      mutate(percent = to.pct(n)) %>%
      mutate(Party = "Total")
    
    summary.data <- bind_rows(summary.data, group.totals)
  }
  
  # Create the crosstab
  crosstab <- summary.data %>%
    group_by(Party) %>%
    mutate(percent = to.pct(n)) %>%
    pivot_wider(names_from = Group, values_from = c(n, percent), values_fill = 0, names_glue = "{Group}_{.value}")
  
  group.cols <- sort(colnames(crosstab)[2:ncol(crosstab)])
  
  crosstab <- crosstab %>%
    select(Party, all_of(group.cols))
  
  if (totals) {
    party.totals <- summary.data %>% 
      group_by(Party) %>% 
      filter(Party != "Total") %>%
      summarise(n = sum(n), .groups = "drop") %>% 
      ungroup() %>%
      mutate(percent = to.pct(n)) %>%
      rename(
        Total_n = "n",
        Total_percent = "percent"
      )
    
    crosstab <- left_join(crosstab, party.totals, by = "Party") %>%
      mutate(Total_n = if_else(Party == "Total", sample.size, Total_n)) %>%
      mutate(Total_percent = if_else(Party == "Total", 100, Total_percent))
  }

  # Needs to be after select (and possibly immediately before return), 
  #  because dplyr currently strips attributes
  # See: https://github.com/tidyverse/dplyr/issues/5294
  group.list <- sort(unique(as.character(summary.data$Group)))
  attr(crosstab, "group.list") <- group.list  
    
  crosstab
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
    fct_count(data[[var.name]]) %>%
      mutate("Recoded To" = case_when(
        f %in% cat.defs[[var.name]]$Missing ~ 'Missing',
        f %in% cat.defs[[var.name]]$Other ~ 'Other',
        TRUE ~ ''
      )) %>%
      rename(
        "Category" = f,
        "N" = n
      )
  }) %>% set_names(main.vars)
}

get.data.src.info <- function(data, data.def) {
  countries <- data.def$skip.countries
  countries$included <- data %>% filter(! Country %in% global.country.skip) %>% distinct(Country) %>% pull(Country)
  
  questions <- map(main.vars, function(v) {
    if (has_name(data.def, 'question.text') && has_name(data.def$question.text, v))
      return(data.def$question.text[[v]])
    else
      attr(data[[v]], "label")
  }) %>% set_names(main.vars)
  
  list(
    questions = questions,
    countries = countries
  )
}

# This function removes rows from a country data frame that will not be used in
#  the analysis. i.e. rows for party or group that are coded as Missing/Other
#  *and* rows for parties/groups that are less than 2% of the *total* sample
# Note, this is done based on one particular group (Ethnicity/Language/Religion)
#  at a time, since someone could be a member of a language group that is smaller
#  than 2%, but be a member of a large religion group. We would want to keep this
#  person in the analysis for religion, but not for language
drop.rows.from.country.data <- function(d, group.var, weighted = F) {
  weights <- NULL
  if (weighted)
    weights <- d$Weight
  
  d <- d %>% 
    mutate(across(all_of(c("Party", group.var)), ~fct_lump_prop(fct_drop(.x), 0.02, w = weights))) %>%
    filter(
      ! .data[[group.var]] %in% cats.to.drop & ! Party %in% cats.to.drop
    )
  
  d
}

calc.correlations <- function(d, drop.cats = F, use.weights = F) {
  country <- unique(d$Country)
  #cat("Calc correlations for", country, "\n")

  tau <- map_dfr(group.names, function(var) {
    d.g <- d 
    
    if (drop.cats)
      d.g <- drop.rows.from.country.data(d.g, var)
    
    n.eff <- nrow(d.g)
    
    empty.res <- tibble(question = var, tau = NA, N.eff = n.eff, gallagher = NA)
    
    if (n.eff <= 200 || length(unique(d.g[[var]])) <= 1) {
      return(empty.res)
    }
    
    d.g <- d.g %>% mutate(
      Party = fct_drop(Party),
      "{var}" = fct_drop(.data[[var]])
    )
    
    wt.var = NULL
    if (use.weights)
      wt.var = "Weight"
    
    assoc <- NULL
    try({
      assoc <- suppressWarnings(pw.assoc(as.formula(paste(var, "~ Party")), d.g, out.df = T, weights = wt.var))
    })
    
    if (is.null(assoc))
      return(empty.res)
    
    res <- assoc %>%
      select(tau) %>%
      mutate(across(everything(), ~round(.x, digits = 3))) %>%
      mutate(question = var, N.eff = n.eff) %>%
      mutate(gallagher = calc.gallagher(d.g, var, drop.cats = F))
    
    huber <- do.huber.calcs(d.g, var, drop.cats = F)
    
    bind_cols(res, huber)
  })
  
  tau %>% select(question, everything()) %>%
    mutate(across(tau, ~ifelse(is.nan(.x) | .x == -Inf, NA, .x))) %>%
    remove_rownames()
}

# Calculate the 'group basis' (i.e. group with highest GK Tau correlation)
calc.group.basis <- function(cor, ret.max.val = F) {
  if (all(is.na(cor$tau)))
    return (NA)
  
  max.val <- cor %>% 
    summarise(max.tau = max(tau, na.rm = T)) %>%
    pull(max.tau)
  
  if (ret.max.val)
    return (max.val)
  
  cor %>% filter(tau == max.val) %>% 
    slice_head() %>%
    pull(question)
}

calc.gallagher <- function(d, group.to.use, max.groups = NULL, drop.cats = T, loosemore = F) {
  if (is.na(group.to.use))
    return (NA)
  
  # Remove Missing/Other categories from Party and Grouping vars
  if (drop.cats)
    d <- drop.rows.from.country.data(d, group.to.use)
  
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
    rename(group = group.to.use)

  party.sizes.by.grp <- tabyl(d, Party, .data[[group.to.use]], show_missing_levels = F) %>% adorn_percentages()
  
  party.sizes <- tabyl(d, Party, show_missing_levels = F)
  
  gallagher.impl(party.sizes.by.grp, grp.sizes, party.sizes, loosemore = loosemore)
  
}

gallagher.impl <- function(party.sizes.by.grp, grp.sizes, party.sizes, loosemore = F) {
  grp.sizes <- grp.sizes %>%
    mutate(percent = percent*100)
  
  # Create lookup of group sizes
  grp.size.list <- map(grp.sizes$group, ~grp.sizes %>% filter(group == .x) %>% pull(percent)) %>%
    set_names(grp.sizes$group)
  
  # Calculate unweighted values for each party
  res <- party.sizes.by.grp %>% 
    pivot_longer(-Party) %>%
    mutate(value = as.numeric(value)*100 - unlist(grp.size.list[name]))
  
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
  res.wt <- party.sizes %>%
    inner_join(res, by = "Party") %>%
    mutate(total = total * percent)
  
  res.wt %>% 
    ungroup() %>% 
    summarise(total = sum(total)) %>%
    pull(total)
}

do.huber.calcs <- function(d, group.to.use, drop.cats = F) {
  if (is.na(group.to.use))
    return (NA)
  
  # Remove Missing/Other categories from Party and Grouping vars
  if (drop.cats)
    d <- drop.rows.from.country.data(d, group.to.use)
  
  group.sizes <- tabyl(d, {{group.to.use}}, show_missing_levels = F)
  party.sizes <- tabyl(d, Party, show_missing_levels = F)
  group.sizes.by.pty <- d %>% group_by(.data[[group.to.use]], Party) %>% count() %>% group_by(.data[[group.to.use]]) %>% mutate(pct = n / sum(n)) %>% select(-n)
  party.sizes.by.grp <- d %>% group_by(.data[[group.to.use]], Party) %>% count() %>% group_by(Party) %>% mutate(pct = n / sum(n)) %>% select(-n)
  
  # Calculate differences in party support between each pair of groups
  rT <- expand_grid(unique(d[[group.to.use]]), unique(d[[group.to.use]]), unique(d$Party), .name_repair = "minimal") %>%
    set_names("g1", "g2", "p") %>%
    left_join(group.sizes.by.pty, by = c('g1' = group.to.use, 'p' = 'Party')) %>%
    left_join(group.sizes.by.pty, by = c('g2' = group.to.use, 'p' = 'Party')) %>%
    mutate(across(c(pct.x, pct.y), ~if_else(is.na(.x), 0, .x))) %>%
    mutate(rT.init = pct.x - pct.y) %>%
    mutate(rT = rT.init^2)

  # Sum the differences by group dyad
  rT.sum <- rT %>% group_by(g1, g2) %>% summarise(rT.orig = sum(rT), .groups = "drop") %>%
    mutate(rT = sqrt(0.5*rT.orig))
  
  # Adjust differences by group size
  rT.sum <- rT.sum %>%
    inner_join(group.sizes, by = c('g1' = group.to.use)) %>%
    rename('g1.group.sizes' = percent) %>%
    inner_join(group.sizes, by = c('g2' = group.to.use)) %>%
    rename('g2.group.sizes' = percent) %>%
    mutate(VF = rT*g1.group.sizes*g2.group.sizes) %>%
    mutate(VP = rT*g1.group.sizes*g2.group.sizes^2)
  
  # Calculate differences in group support between each pair of parties
  rP <- expand_grid(unique(d$Party), unique(d$Party), unique(d[[group.to.use]]), .name_repair = "minimal") %>%
    set_names("p1", "p2", "g") %>%
    left_join(party.sizes.by.grp, by = c('p1' = 'Party', 'g' = group.to.use)) %>%
    left_join(party.sizes.by.grp, by = c('p2' = 'Party', 'g' = group.to.use)) %>%
    mutate(across(c(pct.x, pct.y), ~if_else(is.na(.x), 0, .x))) %>%
    mutate(rP.init = pct.x - pct.y) %>%
    mutate(rP = rP.init^2)
  
  rP.sum <- rP %>% group_by(p1, p2) %>% summarise(rP.orig = sum(rP), .groups = "drop") %>%
    mutate(rP = sqrt(0.5*rP.orig))
  
  # Adjust differences by party size
  rP.sum <- rP.sum %>%
    inner_join(party.sizes, by = c('p1' = 'Party')) %>%
    rename('p1.party.sizes' = percent) %>%
    inner_join(party.sizes, by = c('p2' = 'Party')) %>%
    rename('p2.party.sizes' = percent) %>%
    mutate(PVF = rP*p1.party.sizes*p2.party.sizes) %>%
    mutate(PVP = rP*p1.party.sizes*p2.party.sizes^2)
  
  list(
    VF  = rT.sum %>% ungroup() %>% summarise(VF = sum(VF)) %>% pull(VF),
    VP  = rT.sum %>% ungroup() %>% summarise(VP = sum(VP)*4) %>% pull(VP),
    PVF = rP.sum %>% ungroup() %>% summarise(PVF = sum(PVF)) %>% pull(PVF),
    PVP = rP.sum %>% ungroup() %>% summarise(PVP = sum(PVP)*4) %>% pull(PVP)
  )

}

# Calculate a DF summarising all countries
calc.summary.data <- function(res) {
  map_dfr(res, function(country.data) {
    orig.sum.data <- country.data$Summary
    #cat(orig.sum.data$general$ID, "\n")
    
    sum <- orig.sum.data$general
    sum$`Data Source Orig` <- NULL
    sum$cor.nomiss <- calc.group.basis(orig.sum.data$cor.nomiss, ret.max.val = T)
    sum$Gallagher <- round(sum$Gallagher, 2)
    sum$`Loosmore Hanby` <- round(sum$`Loosmore Hanby`, 2)
    sum$PVF <- round(sum$PVF, 2)
    sum$PVP <- round(sum$PVP, 2)
    
    # Add summary columns indicating whether the group variable is 'available'.
    #  Available is defined as the correlations were able to be calculated
    #  (so includes, eg., cases where there was only one group)
    available <- country.data$Summary$cor.nomiss %>% 
      mutate(available = !is.na(tau)) %>% 
      select(question, available) %>% 
      pivot_wider(names_from = question, values_from = available)
    sum <- bind_cols(sum, available)

    if (is.na(sum$cor.nomiss)) {
      sum$total.included <- NA
      sum$total.included.pct <- NA
      sum$party.missing <- NA
      sum$party.missing.pct <- NA
      sum$group.missing <- NA
      sum$group.missing.pct <- NA
      
      return(sum)
    }
        
    main.summary.data <- country.data[[sum$`Group Basis`]]
    
    sum$total.included <- orig.sum.data$cor.nomiss %>%
      filter(question == sum$`Group Basis`) %>%
      pull(N.eff)
    sum$total.included.pct <- sum$total.included / sum$`Sample Size`
    
    sum$party.missing <- main.summary.data %>% 
      filter(Party %in% cats.to.drop) %>% 
      summarise(n = sum(n)) %>% 
      pull(n)
    
    sum$party.missing.pct <- sum$party.missing / sum$`Sample Size`
    
    sum$group.missing <- main.summary.data %>% 
      filter(Group %in% cats.to.drop) %>% 
      summarise(n = sum(n)) %>% 
      pull(n)
    
    sum$group.missing.pct <- sum$group.missing / sum$`Sample Size`
    
    sum
  })
}

get.group.size.summary <- function(res) {
  # Add in group sizes for 3 largest groups for each country,
  #  as well as breakdowns for each Party/Main Group combo
  map_dfr(res, function(country.data) {
    if (is.na(country.data$Summary$general$`Group Basis`)) {
      return(NULL)
    }
    
    summary.data <- country.data[[country.data$Summary$general$`Group Basis`]]
    main.crosstab <- gen.crosstab(summary.data, drop.cats = T)
    
    gs.row <- country.data$Summary$general %>%
      select(Country, `Data Source`, Year, `Group Basis`)
    
    gs <- main.crosstab %>% 
      pivot_longer(-Party, names_to = "Group") %>% 
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

get.excel.summary.sheet <- function(res) {
  summary.sheet <- res$summary %>%
    mutate(across(ends_with('.pct'), ~set.class('percentage', .))) %>%
    select(-Religion, -Ethnicity, -Language)

  return(summary.sheet)
}

get.max.parties <- function(group.sizes) {
  length(names(group.sizes)[str_detect(names(group.sizes), "^Party.Grp")])
}

write.divided.xlsx <- function(res, file = "Divided/output/divided_crosstabs.xlsx") {
  summary.sheet <- get.excel.summary.sheet(res) %>%
    select(-ID) %>%
    arrange(desc(cor.nomiss)) %>%
    select(Country, `Data Source`, Year, `Sample Size`, `Group Basis`, cor.nomiss, everything())
  
  options("openxlsx.numFmt" = NULL)
  wb <- createWorkbook()
  
  hs1 <- createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- createStyle(textDecoration = "bold")
  
  # Add summary sheet with 2 header rows
  addWorksheet(wb, "Summary")
  outer.headers <- c("", "", "", "", "", "", "", "", "", "", 
                     "Included in Group", "", "Party Missing", "", "Group Missing")
  writeData(wb, "Summary", data.frame(t(outer.headers)), startRow = 1, startCol = 1, colNames = F, rowNames = F)
  addStyle(wb, sheet = "Summary", hs2, rows = 1, cols = 1:length(outer.headers))
  mergeCells(wb, "Summary", cols = 11:12, rows = 1)
  mergeCells(wb, "Summary", cols = 13:14, rows = 1)
  mergeCells(wb, "Summary", cols = 15:16, rows = 1)
  
  summary.headers <- c("Country", "Data Source", "Survey Year", "Sample Size", "Group Basis", "Correlation", "Gallagher", "Loosemore Hanby", 
                       "PVP", "PVF", "(N)", "(%)", "(N)", "(%)", "(N)", "(%)")
  
  writeData(wb, "Summary", data.frame(t(summary.headers)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  setColWidths(wb, sheet = "Summary", cols = 1:length(summary.headers), widths = "auto")
  addStyle(wb, sheet = "Summary", hs2, rows = 2, cols = 1:length(summary.headers))
  
  writeData(wb, "Summary", summary.sheet, startRow = 3, colNames = F, rowNames = F)
  
  # Add group sizes sheet
  addWorksheet(wb, "Group Sizes")
  group.sizes <- res$group.sizes
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
  
  for (country in sort(names(res$crosstabs))) {
    country.sht <- str_replace(country, "Palestinian Territories", "Palestine")
    country.sht <- str_trunc(country.sht, 31)
    
    addWorksheet(wb, country.sht)
    
    startRow <- 1
    for (table.name in group.names) {
      table <- gen.crosstab(res$crosstabs[[country]][[table.name]])
      
      headers <- c("Party", attr(table, "group.list"))
      
      if (is.null(table))
        next()
      
      writeData(wb, country.sht, table.name, startCol = 1, startRow = startRow, rowNames = F)
      addStyle(wb, sheet = country.sht, hs1, rows = startRow, cols = 1)
      
      table <- table %>% 
        mutate(across(ends_with("percent"), ~.x/100)) %>%
        mutate(across(ends_with("percent"), ~set.class('percentage', .x))) 
      
      headerCol <- 1
      for (header in headers) {
        writeData(wb, country.sht, header, startCol = headerCol, startRow = startRow + 1)
        addStyle(wb, sheet = country.sht, hs2, rows = startRow + 1, cols = headerCol)
        
        if (header != "Party") {
          mergeCells(wb, country.sht, cols = headerCol:(headerCol+1), rows = startRow + 1)
          headerCol <- headerCol+1
        }
        
        headerCol <- headerCol+1
      }
      
      writeData(wb, country.sht, table, startCol = 1, startRow = startRow + 2, rowNames = F, colNames = F)
      setColWidths(wb, sheet = country.sht, cols = 1, widths = "auto")
      setColWidths(wb, sheet = country.sht, cols = 2:ncol(table), widths = "10")
      
      startRow <- nrow(table) + 3 + startRow
    }
    
    writeData(wb, country.sht, "Statistics", startCol = 1, startRow = startRow)
    addStyle(wb, sheet = country.sht, hs1, rows = startRow, cols = 1)
    
    writeData(wb, country.sht, "Sample Size", startCol = 1, startRow = startRow+2)
    writeData(wb, country.sht, res$crosstabs[[country]]$Summary$general$`Sample Size`, startCol = 2, startRow = startRow+2)
    
    writeData(wb, country.sht, "Correlations", startCol = 1, startRow = startRow+4)
    addStyle(wb, sheet = country.sht, hs2, rows = startRow+4, cols = 1)
    writeData(wb, country.sht, res$crosstabs[[country]]$Summary$cor, startCol = 2, startRow = startRow+5)
    
    writeData(wb, country.sht, "Correlations (Party = None/Missing/DK removed)", startCol = 1, startRow = startRow+9)
    addStyle(wb, sheet = country.sht, hs2, rows = startRow+9, cols = 1)
    
    writeData(wb, country.sht, res$crosstabs[[country]]$Summary$cor.nomiss, startCol = 2, startRow = startRow+11)
  }
  
  saveWorkbook(wb, file, overwrite = T)
}
