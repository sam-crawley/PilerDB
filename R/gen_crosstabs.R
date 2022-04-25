library(tidyverse)
library(openxlsx)
library(here)
library(StatMatch)
library(rlist)
library(stringi)

summary.group.size <- 5

# These "countries" should *always* be skipped
global.country.skip <- c("Hong Kong SAR China", "Macao SAR China", "Puerto Rico")

cats.to.drop <- c("Missing", "Other")

version.maj = 1
version.min = 0

# Generate crosstabs for all datasets
gen.all.crosstabs <- function(ids.to.load = NULL, existing.data = NULL, save.output = F, calc.summaries = T, full.version = F) {
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
  
  tabs <- add.warning.flags(tabs)
  
  version <- paste(c(version.maj, version.min), collapse = ".")
  
  if (! full.version) {
    version <- paste0(version, ".", str_remove_all(Sys.Date(), "-"))
  }
  
  res <- list(
    version = version,
    crosstabs = tabs,
    cat.sum = cat.sum,
    data.src.info = data.src.info
  )
  
  if (calc.summaries) {
    res$summary <- calc.summary.data(res$crosstabs)
    res$summary.by.group <- map(c(group.names), ~ calc.summary.data(res$crosstabs, group.to.use = .x)) %>% set_names(group.names)
    res$group.sizes <- get.group.size.summary(res$crosstabs)
    res$group.sizes.by.group <- map(c(group.names), ~ get.group.size.summary(res$crosstabs, group.to.use = .x)) %>% set_names(group.names)
    res$max.parties <- get.max.parties(res$group.sizes)
  }
  
  if (save.output) {
    write_rds(res, "output/divided.rds")
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
  
  # Calculate the counts of available parties/groups
  avail.counts <- calc.avail.counts(d, with.removals = T)
  avail.counts.orig <- calc.avail.counts(d, with.removals = F)
  
  if (is.null(year))
    year <- max(as.numeric(d$Year), na.rm = T)
  
  cor = calc.all.indices(d, tables)
  cor.wt = calc.all.indices(d, tables, weighted = T)
  cor.nomiss = calc.all.indices(d, tables, drop.cats = T)
  cor.nomiss.wt = calc.all.indices(d, tables, drop.cats = T, weighted = T)
  
  group.basis <- calc.group.basis(cor.nomiss.wt)
  
  gallagher <- NA
  loosmore <- NA
  pvp <- NA
  pvf <- NA
  
  if (! is.na(group.basis)) {
    cor.vals <- cor.nomiss.wt %>% filter(group == group.basis)
    gallagher <- cor.vals$gallagher
    loosmore <- cor.vals$loosmore
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
      'Loosmore Hanby' = loosmore,
      'PVP' = pvp,
      'PVF' = pvf
    ),
    cor = cor,
    cor.wt = cor.wt,
    cor.nomiss = cor.nomiss,
    cor.nomiss.wt = cor.nomiss.wt,
    country.orig = country.orig,
    avail.counts = avail.counts,
    avail.counts.orig = avail.counts.orig
    
  )
  
  tables
}

# Calc summarised group data for a single country
#  This data is stored in the .rds file and can be converted into crosstabs
#  for display purposes
calc.summarised.group.data <- function(data, group.var) {
  if (all( data[group.var] == "Missing" ) || all( data$Party == "Missing" )) {
    return (NA)
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

# Regenerate the indices data for all countries, without re-generating the crosstabs, etc.
#  This is much quicker than generating everything from scratch
regen.all.indicies <- function(tabs) {
  map(tabs$crosstabs, function(country.data) {
    #cat("Regen", country.data$Summary$general$ID, "\n")
    
    summary.data.list <- country.data[group.names]
    
    country.data$Summary$cor = update.summary.indices(country.data$Summary$cor, summary.data.list)
    country.data$Summary$cor.wt = update.summary.indices(country.data$Summary$cor.wt, summary.data.list, weighted = T)
    country.data$Summary$cor.nomiss = update.summary.indices(country.data$Summary$cor.nomiss, summary.data.list, drop.cats = T)
    country.data$Summary$cor.nomiss.wt = update.summary.indices(country.data$Summary$cor.nomiss.wt, summary.data.list, drop.cats = T, weighted = T)
    
    group.basis <- calc.group.basis(country.data$Summary$cor.nomiss.wt)
    
    gallagher <- NA
    loosmore <- NA
    pvp <- NA
    pvf <- NA
    
    if (! is.na(group.basis)) {
      cor.vals <- country.data$Summary$cor.nomiss.wt %>% filter(group == group.basis)
      gallagher <- cor.vals$gallagher
      loosmore <- cor.vals$loosmore
      pvp <- cor.vals$PVP
      pvf <- cor.vals$PVF
    }
    
    country.data$Summary$general$`Group Basis` = group.basis
    country.data$Summary$general$`Gallagher` = gallagher
    country.data$Summary$general$`Loosmore Hanby` = loosmore
    country.data$Summary$general$`PVP` = pvp
    country.data$Summary$general$`PVF` = pvf
    
    country.data
  })
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
  if (! is.data.frame(summary.data))
    return (NA)
  
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

# Calculate the counts of groups/parties available in a country survey
calc.avail.counts <- function(d, with.removals = F) {
  map(main.vars, function(var.name) {
    avail <- d %>%
      group_by(.data[[var.name]]) %>% 
      summarise(n = n(), .groups = "drop") %>% 
      mutate(p = n / sum(n))
    
    if (with.removals) {
      avail <- avail %>%
        filter(p >= 0.02 & ! .data[[var.name]] %in% cats.to.drop)
    }
    else {
      # Treat 'Missing' as a "special case", i.e. we don't count it as
      #  an available group even *before* removals
      avail <- avail %>%
        filter(.data[[var.name]] != "Missing")
    }
    
    nrow(avail)
  }) %>% set_names(main.vars)
}

gen.category.summary <- function(data, cat.defs) {
  # Internal function to find which variables the 
  #  categories have been recoded to
  find.recodes <- function(var.name, cats) {
    map_chr(cats, function(cat) {
      ret <- map(names(cat.defs[[var.name]]), function(recode_cat) {
        if (cat %in% cat.defs[[var.name]][[recode_cat]])
          return (recode_cat)
        
        return (NULL)
      })
      
      ret <- compact(ret)
      
      if (is_empty(ret))
        return ('')
      
      as.character(ret)
    })
  }
  
  map(main.vars, function(var.name) {
    fct_count(data[[var.name]]) %>%
      mutate("Recoded To" = find.recodes(var.name, f)) %>%
      rename(
        "Category" = f,
        "N" = n
      )
  }) %>% set_names(main.vars)
}

get.data.src.info <- function(data, data.def) {
  questions <- map(main.vars, function(v) {
    if (has_name(data.def, 'question.text') && has_name(data.def$question.text, v))
      return(data.def$question.text[[v]])
    else
      attr(data[[v]], "label")
  }) %>% set_names(main.vars)
  
  modules <- NA
  if (has_name(data.def, "wave_var"))
    modules <- sort(unique(data[[data.def$wave_var]]))
  
  list(
    questions = questions,
    modules = modules
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

calc.all.indices <- function(country.data, sum.dfs, drop.cats = F, weighted = F) {
  #country <- unique(country.data$Country)
  #cat("Calc indices for ", country, "\n")
  
  indices <- map_dfr(group.names, function(group) {
    summary.data <- sum.dfs[[group]]
    
    calc.indices(country.data, summary.data, group, drop.cats = drop.cats, weighted = weighted)
  })

  indices %>%
    mutate(across(tau, ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x))) %>%
    mutate(across(-group, ~round(.x, digits = 3))) %>%
    remove_rownames()
}

# Calculate the 'group basis' (i.e. group with highest GK Tau correlation)
calc.group.basis <- function(cor) {
  if (all(is.na(cor$tau))) {
    # If all tau's are NA, go by Gallagher instead (if available)
    if (has_name(cor, 'gallagher'))
      return (cor %>% slice_max(gallagher, with_ties = F) %>% pull(group))
    else
      # Gallagher wasn't calculated, so no way of determining group basis
      return (NA)
  }
  
  cor %>% 
    slice_max(tau, with_ties = F) %>% 
    pull(group)
}

# Calculate a DF summarising all countries
calc.summary.data <- function(res, group.to.use = NULL) {
  map_dfr(res, function(country.data) {
    orig.sum.data <- country.data$Summary
    #cat(orig.sum.data$general$ID, "\n")
    
    group.basis.selected <- F
    
    sum <- orig.sum.data$general %>%
      select(-`Data Source Orig`, -warning.flags.details)
    
    if (is.null(group.to.use)) {
      group.to.use <- sum$`Group Basis`
    }
    else {
      group.basis.selected <- T
      sum$`Group Basis` <- group.to.use
    }
      
    stats <- orig.sum.data$cor.nomiss.wt %>%
        filter(group == group.to.use)
    
    sum$cor.nomiss <- NA
    if (nrow(stats) > 0) {
      sum$cor.nomiss <- round(stats$tau, 2)
        
      if (has_name(stats, 'gallagher')) {
        sum$Gallagher <- stats$gallagher
        sum$`Loosmore Hanby` <- stats$loosmore
        sum$PVF <- stats$PVF
        sum$PVP <- stats$PVP
        
        sum <- sum %>% mutate(across(c(Gallagher, `Loosmore Hanby`, PVF, PVP), ~ round(.x, 2)))
      }
    }
    
    # Add summary columns indicating whether the group variable is 'available'.
    #  Available is defined as the correlations were able to be calculated
    #  (so includes, eg., cases where there was only one group)
    available <- country.data$Summary$cor.nomiss.wt %>% 
      mutate(available = {if ("gallagher" %in% names(.)) ! is.na(gallagher) else F}) %>%
      select(group, available) %>% 
      pivot_wider(names_from = group, values_from = available)
    sum <- bind_cols(sum, available)
    
    sum$excluded <- NA
    
    # Decide if this country is considered "excluded" or "included".
    #  The logic is slightly different, depending on whether the group basis
    #  was passed to this function
    is.excluded = F
    if (group.basis.selected)
      is.excluded <- is.na(sum$Gallagher)
    else
      is.excluded <- is.na(sum$`Group Basis`)

    if (is.excluded) {
      sum$total.included <- NA
      sum$total.included.pct <- NA
      sum$party.missing <- NA
      sum$party.missing.pct <- NA
      sum$group.missing <- NA
      sum$group.missing.pct <- NA
      
      suppressWarnings({
        if (orig.sum.data$avail.counts.orig$Party == 0)
          sum$excluded <- "No party data"
        else if (orig.sum.data$avail.counts$Party == 0)
          sum$excluded <- "No parties after removals"
        else if (all(orig.sum.data$avail.counts.orig[group.names] == 0))
          sum$excluded <- "No group data"
        else if (all(orig.sum.data$avail.counts[group.names] == 0))
          sum$excluded <- "No groups after removals"
        else if (group.basis.selected) {
          stats <- orig.sum.data$cor.nomiss.wt %>% filter(group == all_of(group.to.use))
          
          if (orig.sum.data$avail.counts[[group.to.use]] == 0 | ! has_name(stats, 'group'))
            sum$excluded <- paste(group.to.use, "not available")
          else if (stats$n.eff <= 200)
            sum$excluded <- "N <= 200 after removals"
        }
        else if (max(orig.sum.data$cor.nomiss.wt$n.eff, na.rm = T) <= 200)
          sum$excluded <- "N <= 200 after removals"
      })
      
      return(sum)
    }
    
    main.summary.data <- country.data[[group.to.use]]
    
    sum$total.included <- orig.sum.data$cor.nomiss.wt %>%
      filter(group == group.to.use) %>%
      pull(n.eff)
    sum$total.included.pct <- sum$total.included / sum$`Sample Size`
    
    parties.to.drop <- find.groups.to.drop(main.summary.data, "Party")
    
    sum$party.missing <- main.summary.data %>% 
      filter(! Party %in% c(cats.to.drop, parties.to.drop)) %>%
      summarise(n = sum(n)) %>% 
      pull(n)
    
    sum$party.missing.pct <- sum$party.missing / sum$`Sample Size`
    
    groups.to.drop <- find.groups.to.drop(main.summary.data, "Group")
    
    sum$group.missing <- main.summary.data %>% 
      filter(! Group %in% c(cats.to.drop, groups.to.drop)) %>%
      summarise(n = sum(n)) %>% 
      pull(n)
    
    sum$group.missing.pct <- sum$group.missing / sum$`Sample Size`
    
    sum
  })
}

get.group.size.summary <- function(res, group.to.use = NULL) {
  # Add in group sizes for 3 largest groups for each country,
  #  as well as breakdowns for each Party/Main Group combo
  map_dfr(res, function(country.data) {
    if (is.null(group.to.use))
      group.basis <- country.data$Summary$general$`Group Basis`
    else
      group.basis <- group.to.use
    
    if (is.na(group.basis) || is.null(group.basis))
      return(NULL)
    
    summary.data <- config.summary.data(country.data[[group.basis]], drop.cats = T)
    
    if (! is.data.frame(summary.data))
      return(NULL)

    gs.row <- country.data$Summary$general %>%
      select(Country, `Data Source`, Year) %>%
      mutate(`Group Basis` = group.basis)
    
    gs <- summary.data %>% 
      group_by(Group) %>% 
      summarise(n = sum(n)) %>% 
      filter(n > 0) %>%
      slice_max(n, n=5, with_ties = F)      

    main.groups <- gs$Group

    for (row in 1:summary.group.size) {
      sum.row <- gs[row, ]
      gs.row <- suppressMessages(bind_cols(gs.row, sum.row))
    }
    
    party.group.sizes <- summary.data %>% 
      filter(Group %in% main.groups) %>% 
      pivot_wider(names_from = Group, values_from = n) %>%
      rowwise() %>% 
      mutate(Total = sum(c_across(-Party))) %>%
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
    
    # Workaround for only 1 party - columns need to be renamed
    # XXX: this assumes there are 5 groups...
    #  Probably need to re-write all this code to rely on pivot_wider
    if (nrow(party.group.sizes) == 1) {
      gs.row <- gs.row %>% rename(
        "Party.Grp...15" = "Party.Grp",
        "Total...16" = "Total",
        "Group 1...17" = "Group 1",
        "Group 2...18" = "Group 2",
        "Group 3...19" = "Group 3",
        "Group 4...20" = "Group 4",
        "Group 5...21" = 'Group 5'
      ) %>% mutate(
        "Party.Grp...22" = NA,
        "Total...23" = NA,
        "Group 1...24" = NA,
        "Group 2...25" = NA,
        "Group 3...26" = NA,
        "Group 4...27" = NA,
        "Group 5...28" = NA
      )
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

gen.spreadsheets <- function(tabs) {
  write.divided.xlsx(tabs, include.crosstabs = F, file = "output/divided_summary.xlsx")
  write.divided.xlsx(tabs, include.summary = F, include.group.sizes = F, include.summary.by.group = F)
}

write.divided.xlsx <- function(res, include.summary = T, include.summary.by.group = T, include.group.sizes = T, include.crosstabs = T, include.group.sizes.by.group = T,
                               file = "output/divided_crosstabs.xlsx") {
  options("openxlsx.numFmt" = NULL)
  wb <- createWorkbook()
  
  hs1 <- createStyle(textDecoration = "bold", fontSize = 14)
  hs2 <- createStyle(textDecoration = "bold")
  
  addWorksheet(wb, "About")
  
  spreadsheet.contents <- c("Summaries", "Summaries by group", "Group Sizes", "Group sizes by group", "Country Crosstabs")
  contents.included <- c(include.summary, include.summary.by.group, include.group.sizes, include.group.sizes.by.group, include.crosstabs)
  
  about.data <- c(
    paste0("Divided Societies DB v", tabs$version), 
    paste("Generated", Sys.time()),
    paste("This spreadsheet includes: ", paste0(spreadsheet.contents[contents.included], collapse = ", "))
  )
  
  writeData(wb, "About", about.data, startRow = 1, startCol = 1)
  
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
      
      addWorksheet(wb, country.sht)
      
      startRow <- 1
      for (table.name in group.names) {
        table <- res$crosstabs[[country]][[table.name]]
        
        if (! is.data.frame(table))
          next()
        
        table <- gen.crosstab(table)
        
        headers <- c("Party", attr(table, "group.list"))
  
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
  }
  
  saveWorkbook(wb, file, overwrite = T)
}

write.excel.summary.tab <- function(wb, summary.data, tab.name = "Summary", included.excluded.col = T) {
  summary.sheet <- summary.data %>%
    mutate(across(ends_with('.pct'), ~set.class('percentage', .))) %>%
    select(-Religion, -Ethnicity, -Language, -ID, -`Loosmore Hanby`) %>%
    arrange(desc(cor.nomiss)) %>%
    select(Country, `Data Source`, Year, `Sample Size`, `Group Basis`, cor.nomiss, everything()) %>%
    relocate(warning.flags, .after = everything())
  
  hs2 <- createStyle(textDecoration = "bold")
  
  addWorksheet(wb, tab.name)
  outer.headers <- c("", "", "", "", "", "", "", "", "", "", "",
                     "Included in Group", "", "Party Missing", "", "Group Missing")
  writeData(wb, tab.name, data.frame(t(outer.headers)), startRow = 1, startCol = 1, colNames = F, rowNames = F)
  addStyle(wb, sheet = tab.name, hs2, rows = 1, cols = 1:length(outer.headers))
  mergeCells(wb, tab.name, cols = 12:13, rows = 1)
  mergeCells(wb, tab.name, cols = 14:15, rows = 1)
  mergeCells(wb, tab.name, cols = 16:17, rows = 1)
  
  summary.headers <- c("Country", "Data Source", "Survey Year", "Sample Size", "Group Basis", "Correlation", "ATT-Pol",
                       "PVP", "PVF", "Exclusion Reason", "(N)", "(%)", "(N)", "(%)", "(N)", "(%)", "Warning Flags")
  
  writeData(wb, tab.name, data.frame(t(summary.headers)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  setColWidths(wb, sheet = tab.name, cols = 1:length(summary.headers), widths = "auto")
  addStyle(wb, sheet = tab.name, hs2, rows = 2, cols = 1:length(summary.headers))
  
  writeData(wb, tab.name, summary.sheet, startRow = 3, colNames = F, rowNames = F)
}

write.excel.group.sizes.tab <- function(wb, group.sizes.data, tab.name = "Group Sizes") {
  addWorksheet(wb, tab.name)
  max.parties <- get.max.parties(group.sizes.data)
  
  hs2 <- createStyle(textDecoration = "bold")
  
  writeData(wb, tab.name, "Largest Groups", startRow = 1, startCol = 5)
  
  for (party.num in 1:max.parties) {
    col <- 4 + (summary.group.size * 2) + ( (party.num-1) * (2 + summary.group.size) ) + 1
    writeData(wb, tab.name, paste("Supporters of Party", party.num), startRow = 1, startCol = col)
  }
  
  group.size.names <- c(
    "Country", "Data Source", "Year", "Group Basis",
    rep(c("Name", "N"), summary.group.size),
    rep(c("Party", "Total N", paste("Group", 1:summary.group.size)), max.parties)
  )
  
  writeData(wb, tab.name, data.frame(t(group.size.names)), startRow = 2, startCol = 1, colNames = F, rowNames = F)
  writeData(wb, tab.name, group.sizes.data, startRow = 3, startCol = 1, colNames = F, rowNames = F)
  
  header.cols <- 4 + (summary.group.size * 2) + ( (max.parties) * (2 + summary.group.size) )
  setColWidths(wb, sheet = tab.name, cols = 1:header.cols, widths = "auto")
  addStyle(wb, sheet = tab.name, hs2, rows = 1, cols = 1:header.cols)
  addStyle(wb, sheet = tab.name, hs2, rows = 2, cols = 1:header.cols)  
}
  
  