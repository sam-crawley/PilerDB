# Functions to generate the PILER DB

summary.group.size <- 5

# These "countries" should *always* be skipped
global.country.skip <- c("Hong Kong SAR China", "Macao SAR China", "Puerto Rico")

cats.to.drop <- c("Missing", "Other", "No Religion")

version.maj = 1
version.min = 0

#' Generate the PILER DB from scratch
#' 
#' This function generates a new PILER DB from the original dataset files. The datasets that will be used
#' are defined by the data_defs (see below for details).
#' 
#' The dataset files themselves are not distributed to this package due to licensing restructions. However
#'   they are all available online for free.
#' 
#' @param ids.to.load A vector of dataset IDs to load into the new DB. If NULL, all datasets are loaded.
#' @param use.existing.data Logical value indicating if an existing version of the PILER DB should be
#'   loaded. If true, any datasets specified in ids.to.load will overwrite the existing data.
#' @param existing.data The existing PILER DB object to use. If NULL, will use the piler object from
#'   the package.
#' @param calc.summaries Logical. If true, summary data is calculated and added to the object returned.
#' @param full.version Logical. If true, this should be considered a "full" version of the DB. If false,
#'   a date string will be appended to the version number.
#' @param datasets.dir Path to original dataset files (used to generate the DB). The default is to look
#'   in the "datasets" directory in the root directory of the current project (found via here::here()).
#' @return A PILER DB (list) object. See \link{piler} for details of the structure. 
#'
#' @section dafe_defs:
#' 
#' These are R files that define of how the datasets should be loaded.
#' 
#' Each dataset file (i.e. survey wave) has a corresponding definition file. The
#' definition file (or "data def") creates two objects:
#' \describe{
#'   \item{data.spec}{A list of parameters about the data file itself (see below for details).}
#'   \item{cat.defs}{A list of category recodes for each group type.}
#' }
#' 
#' The data def files are placed in the "data_defs" directory, which is under "inst" in git
#'   and in the root directory of the installed package. When building the DB, \code{gen.piler.db}
#'   looks for these data definitions, loads the dataset file, does some processing, and adds
#'   the results to the DB.
#'   
#' The file name should be an identifier for the wave (e.g. "WVS7" for World Values Survey 7), followed
#'  by ".R". This identifier will be used throughout the database to identify this wave. The identifier
#'  must be letters followed by numbers/letters. The letters are used to indicated the survey organisation,
#'  and the numbers/letters used to indicate the wave. e.g. for "PEW2009a", "PEW" is the organisation, and
#'  "2009a" is the wave. (The first digit is assumed to be the start of the wave name).
#'   
#' @section data.spec fields:
#' The data.spec should be a list with the following fields (all required, unless otherwise indicated):
#' 
#' \describe{
#'   \item{file.name}{The name of the dataset file to be loaded (without the path). The path is generated
#'   from the dataset identifier, e.g. for "WVS7", the path will be "WVS/WVS7" in the dataset directory.
#'   The dataset directory is set in the call to  \code{gen.piler.db}.}
#'   \item{file.type}{Type of the datafile, currently must be "sav" or "dta".}
#'   \item{file.encoding}{Encoding to use when reading the file, defaults to UTF-8.}
#'   \item{field.def}{Named character vector of fields that will used to generate crosstabs, etc. for the DB. The names
#'     should be Party, Language, Religion, Ethnicity, Year, Weight, Country, and the values should be the corresponding
#'     names of the columns in the data file. The first four indicate the Party/Group data for this data file (can be NA
#'     if not included in this data file). Year is the year of the survey (the max value of this column will be used if
#'     there are multiple values for a given country). Country is the name of the country (which will be converted, see below).
#'     Weight is the survey weights to be applied later. Columns in the dataset will be renamed before further processing is done.}
#'   \item{wave_var}{(Optional) For multiwave datasets, this variable in the data indicates the wave/module.
#'           Data is split based on this variable, and the value of the variable is appended
#'           to the data source ID (meaning each wave appears as a different data source).}
#'   \item{split_by_year}{Logical, default is false. If true, splits out data by the value of the Year variable.
#'                 Some datasets (CSES) contain multiple surveys for the same country,
#'                 so this separates them out.}
#'   \item{country.format}{The original format of the country in the datafile (from countrycode::codelist). 
#'     When building the DB, the value is converted using countrycode to 'country.name' format}
#'   \item{country.dict}{(Optional) country dict to use (passed to the custom_dict parameter of countrycoude())}
#'   \item{country.custom}{(Optional) Names vector to be passed to custom_match parameter of countrycode()}
#'   \item{manual.exclusions}{List of countries that should be excluded from the DB.
#'                  For datasets that use split_by_year, this should be a list of lists, with the key being the
#'                  year, and the value the list of countries to exclude from that year.}
#'   \item{fixups}{(Optional) A function which applies any fixups as the last step of reading the datafile. The only parameter is
#'     the dataframe of the wave. Note, at this point the columns have been renamed, as per field.def.}
#'   \item{pre_fixups}{(Optional) The same as fixups, but this is applied immediately after the datafile is loaded.}
#' }
#' 
#' @section cat.defs structure:
#' 
#' This should be a names list of named lists. The outer list should have names that are one of main variable names
#' (i.e. Party, Religion, Ethnicity, Language), although all are optional. Each of the values should be lists with
#' values being a vector of categories for that variable in the dataset that should be recoded. The name is what those
#' categories will be recoded to (usually "Missing" or "Other").
#' 
#' @export
gen.piler.db <- function(ids.to.load = NULL, use.existing.data = F, existing.data = NULL, calc.summaries = T, full.version = F, datasets.dir = NULL) {
  tabs <- list()
  cat.sum <- list()
  data.src.info <- list()
  
  if (use.existing.data) {
    if (is.null(existing.data))
      existing.data <- piler
    
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
  
  res <- map(data.defs, function(data.def.file) {
    id <- get.data.def.id(data.def.file)
    
    if (! is.null(ids.to.load) & ! id %in% ids.to.load)
        return (NULL)
    
    cat("Processing", data.def.file, "\n")
    
    data.def <- new.env()
    
    source(data.def.file, local = data.def, encoding = "UTF-8")
    
    data <- read.div.data(data.def$data.spec, data.def.file = data.def.file, datasets.dir = datasets.dir)
    
    data.cat.sum <- gen.category.summary(data, data.def$cat.defs)
    src.tabs <- gen.country.crosstabs(data, id, data.def)
    info <- get.data.src.info(data, data.def$data.spec)
    
    return (list(
      id = id,
      cat.sum = data.cat.sum,
      tabs = src.tabs,
      info = info
    ))
    
  })
  
  # Reshape data
  res <- compact(res)
  names <- purrr::modify(res, "id")  %>% as.character()
  
  new.tabs <- purrr::modify(res, "tabs") %>% purrr::flatten()
  tabs <- append(tabs, new.tabs)
  
  new.cat.sum <- purrr::modify(res, "cat.sum") %>% set_names(names)
  cat.sum <- append(cat.sum, new.cat.sum)
  
  data.src.info <- purrr::modify(res, "info") %>% set_names(names)
  
  # XXX: disabled for now, since it's not really used
  #tabs <- add.warning.flags(tabs)
  
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
    res <- calc.all.summaries(res)
  }
  
  res
}

# Produce a data structure for a single dataset that includes crosstabs for each country
#  At this level, we should only be doing summarising that requires access to the original dataset
#  (Because the idea is that the original data will not be available after this point)
gen.country.crosstabs <- function(data, data.source, data.def) {
  data.spec <- data.def$data.spec
  
  if (is.null(data.spec$split.by.year))
    data.spec$split.by.year = F
  
  # Do common processing of dataset
  data <- process.data(data, data.def$cat.defs)
  
  data <- data %>%
    filter(! Country %in% global.country.skip)
  
  # Split data up by country
  split.factor <- list(data$Country)
  if (! is.null(data.spec$wave.var)) {
    split.factor <- rlist::list.append(split.factor, data[[data.spec$wave.var]])
  }
  if (data.spec$split.by.year) {
    split.factor <- rlist::list.append(split.factor, data$Year)
  }
  
  data.by.country <- split(data, split.factor, drop = T)
  
  # Collect params so we can pass to future_map to do the processing for each country
  ct.params <- map(names(data.by.country), function(key) {
    cntry <- key
    data.source.orig = data.source
    year <- NULL
    
    splt <- str_split(key, "\\.", simplify = T)
    cntry <- splt[[1]]
    
    splt.idx <- 2
    if (! is.null(data.spec$wave.var)) {
      data.source <- paste0(data.source, splt[[splt.idx]])
      splt.idx <- splt.idx+1
    }
    if (data.spec$split.by.year) {
      year <- as.integer(splt[[splt.idx]])
    }
    
    excluded = F
    if (! is.null(data.spec$manual.exclusions)) {
      if (is.null(year) && cntry %in% data.spec$manual.exclusions)
        excluded = T
      # For 'split_by_year' datasets, we check manual exclusions for each year
      else if (! is.null(year) && ! is.null(data.spec$manual.exclusions[[as.character(year)]]) && cntry %in% data.spec$manual.exclusions[[as.character(year)]])
        excluded = T
    }
    
    party.question.type <- data.spec$party.question.type
    if (! is.null(data.spec$country.party.question.type) & has_name(data.spec$country.party.question.type, cntry))
      party.question.type <- data.spec$country.party.question.type[[cntry]]
    
    list(
      data = data.by.country[[key]],
      cntry = cntry,
      data.source = data.source,
      data.source.orig = data.source.orig,
      year = year,
      excluded = excluded,
      party.question.type = party.question.type
    )
  })
  
  # Create crosstabs for each country
  # (Produces a list of lists, keyed by country+data.source+year)
  res <- furrr::future_map(ct.params, function(params) {  
    gen.single.country.data(
      params[['data']], 
      params[['cntry']], 
      params[['data.source']], 
      params[['data.source.orig']],
      params[['party.question.type']],
      params[['year']], 
      params[['excluded']]
    )
  })
  
  res <- set_names(res, map_chr(res, ~ .x$Summary$general$ID))

  return(res)
}

gen.single.country.data <- function(d, cntry, data.source, data.source.orig, party.question.type, year = NULL, excluded = F) {
  
  country.orig <- d %>% distinct(Country.orig) %>% pull(Country.orig)
  
  tables <- map(group.names, function(var) {
    calc.summarised.group.data(d, var)
  })
  
  names(tables) <- group.names
  
  # Calculate the counts of available parties/groups
  avail.counts <- calc.avail.counts(d, with.removals = T)
  avail.counts.orig <- calc.avail.counts(d, with.removals = F)
  
  if (is.null(year))
    year <- max(as.integer(d$Year), na.rm = T)
  
  cor = calc.all.indices(d, tables)
  cor.wt = calc.all.indices(d, tables, weighted = T)
  cor.nomiss = calc.all.indices(d, tables, drop.cats = T)
  cor.nomiss.wt = calc.all.indices(d, tables, drop.cats = T, weighted = T)
  
  group.basis <- ifelse(! excluded, calc.group.basis(cor.nomiss.wt), NA_character_)
  
  tables$Summary <- list(
    general = tibble(
      'ID' = paste(cntry, data.source, year),
      'Country' = cntry,
      'Data Source' = data.source,
      'Data Source Orig' = data.source.orig,
      'Year' = year,
      'Sample Size' = nrow(d),
      'Group Basis' = group.basis,
      'Party Question Type' = party.question.type
    ),
    cor = cor,
    cor.wt = cor.wt,
    cor.nomiss = cor.nomiss,
    cor.nomiss.wt = cor.nomiss.wt,
    country.orig = country.orig,
    avail.counts = avail.counts,
    avail.counts.orig = avail.counts.orig,
    manually.excluded = excluded
  )
  
  tables
}

# (Re-)calculate the all summary data from the country crosstabs
calc.all.summaries <- function(res) {
  res$summary <- calc.summary.data(res$crosstabs)
  res$summary.by.group <- map(c(group.names), ~ calc.summary.data(res$crosstabs, group.to.use = .x)) %>% set_names(group.names)
  res$group.sizes <- get.group.size.summary(res$crosstabs)
  res$group.sizes.by.group <- map(c(group.names), ~ get.group.size.summary(res$crosstabs, group.to.use = .x)) %>% set_names(group.names)
  res$max.parties <- get.max.parties(res$group.sizes)
  
  return(res)
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
#  This is quicker than generating everything from scratch
# XXX: not currently working
regen.all.indicies <- function(orig.data, calc.summaries = T) {
  piler.new <- orig.data
  
  piler.new$crosstabs <- furrr::future_map(orig.data$crosstabs, function(country.data) {
    
    summary.data.list <- country.data[group.names]
    
    country.data$Summary$cor = update.summary.indices(country.data$Summary$cor, summary.data.list)
    country.data$Summary$cor.wt = update.summary.indices(country.data$Summary$cor.wt, summary.data.list, weighted = T)
    country.data$Summary$cor.nomiss = update.summary.indices(country.data$Summary$cor.nomiss, summary.data.list, drop.cats = T)
    country.data$Summary$cor.nomiss.wt = update.summary.indices(country.data$Summary$cor.nomiss.wt, summary.data.list, drop.cats = T, weighted = T)
    
    group.basis <- calc.group.basis(country.data$Summary$cor.nomiss.wt)

    country.data$Summary$general$`Group Basis` = group.basis

    country.data
  })
  
  if (calc.summaries) {
    piler.new <- calc.all.summaries(piler.new)
  }
  
  
  
  return (piler.new)
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


#' "Configure" a survey-specific summary data frame
#' 
#' Prepare a "summary" data frame (i.e. the group-specific
#'   data frame in the "crosstabs" section of the piler DB).
#' Drops categories as necessary, and selects the appropriate 'n'
#'   column (weighted or unweighted)
#'   
#' This function generally does not need to be called directly, but can be
#'   useful for doing some kinds of analyses.
#' 
#' @param summary.data The summary data, from 
#'   piler$crosstabs\[\[survey\]\]\[\[group\]\]
#'
#' @param drop.cats Logical, defaults to false. If true, "Missing" and "Other"
#'   categories are dropped, along with any categories less than 2% of the
#'   overall sample size.
#' @param weighted Logical, defaults to false. If true, the "weighted" values
#'   of the crosstabs are used, rather than raw numbers.
#'
#' @export
config.summary.data <- function(summary.data, drop.cats = F, weighted = F) {
  if (! is.data.frame(summary.data))
    return (NA)
  
  # Select the right n column, depending on whether we wanted weighted or
  #  unweighted
  if (weighted) {
    summary.data$n <- summary.data$n.weighted 
    summary.data$n.weighted <- NULL
  }  
  else {
    summary.data$n.weighted <- NULL
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
  summary.data <- config.summary.data(summary.data, drop.cats = drop.cats, weighted = weighted)
  
  if (! is.data.frame(summary.data) || nrow(summary.data) == 0)
    return (NULL)  

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
drop.rows.from.country.data <- function(d, group1.var, group2.var, weighted = F) {
  weights <- NULL
  if (weighted)
    weights <- d$Weight
  
  d <- d %>% 
    mutate(across(all_of(c(group1.var, group2.var)), ~fct_lump_prop(fct_drop(.x), 0.02, w = weights))) %>%
    filter(
      ! .data[[group1.var]] %in% cats.to.drop & ! .data[[group2.var]] %in% cats.to.drop
    )
  
  d
}

calc.all.indices <- function(country.data, sum.dfs, drop.cats = F, weighted = F) {
  indices <- map_dfr(group.names, function(group) {
    summary.data <- sum.dfs[[group]]
    
    calc.indices(country.data, summary.data, group, drop.cats = drop.cats, weighted = weighted)
  })

  indices %>%
    mutate(across(tau, ~ifelse(is.nan(.x) | is.infinite(.x), NA, .x))) %>%
    mutate(across(-group, ~round(as.numeric(.x), digits = 3))) %>%
    remove_rownames()
}

# Calculate the 'group basis' (i.e. group with highest PES)
calc.group.basis <- function(cor) {
  if (has_name(cor, 'pes'))
    return (cor %>% slice_max(pes, with_ties = F) %>% pull(group))
  else
    # PES wasn't calculated, so no way of determining group basis
    return (NA)

}

# Calculate a DF summarising all countries
calc.summary.data <- function(res, group.to.use = NULL) {
  furrr::future_map_dfr(res, function(country.data) {
    orig.sum.data <- country.data$Summary

    group.basis.selected <- F
    
    sum <- orig.sum.data$general %>%
      select(-`Data Source Orig`)
    
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
    sum$PES <- NA
    
    if (nrow(stats) > 0) {
      sum$cor.nomiss <- round(stats$tau, 2)
        
      if (has_name(stats, 'pes')) {
        sum$PES <- stats$pes
        sum$PES.nrm <- stats$pes.nrm
        sum$PES.abs <- stats$pes.abs
        sum$PES.abs.nrm <- stats$pes.abs.nrm
        sum$V <- stats$V
        sum$PVF <- stats$PVF
        sum$PVP <- stats$PVP
        
        # Calculate a mean cross.cutting values from all available values
        sum$cross.cutting <- orig.sum.data$cor.nomiss.wt %>% 
          select(group, starts_with("cc")) %>% 
          pivot_longer(-group) %>% 
          filter((group == "Language" | group == "Religion" & name == "cc.E")) %>% 
          summarise(mean = round(mean(value, na.rm = T), 2)) %>% pull(mean)
        sum$cross.cutting <- ifelse(is.infinite(sum$cross.cutting) | is.nan(sum$cross.cutting), NA, sum$cross.cutting)
        
        sum <- sum %>% mutate(across(c(PES, PES.nrm, PVF, PVP), ~ round(.x, 2)))
      }
    }
    
    # Add summary columns indicating whether the group variable is 'available'.
    #  Available is defined as the correlations were able to be calculated
    #  (so includes, eg., cases where there was only one group)
    available <- country.data$Summary$cor.nomiss.wt %>% 
      mutate(available = {if ("pes" %in% names(.)) ! is.na(pes) else F}) %>%
      select(group, available) %>% 
      pivot_wider(names_from = group, values_from = available)
    sum <- bind_cols(sum, available)
    
    sum$excluded <- NA
    
    # Decide if this country is considered "excluded" or "included".
    #  The logic is slightly different, depending on whether the group basis
    #  was passed to this function
    is.excluded = F
    if (orig.sum.data$manually.excluded)
      is.excluded <- T
    else if (group.basis.selected)
      is.excluded <- is.na(sum$PES)
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
        else if (orig.sum.data$manually.excluded)
          sum$excluded <- "Manually excluded"
      })
      
      return(sum)
    }
    
    main.summary.data <- country.data[[group.to.use]]
    
    total.sample <- sum(main.summary.data$n.weighted)
    
    sum$total.included <- orig.sum.data$cor.nomiss.wt %>%
      filter(group == group.to.use) %>%
      pull(n.eff)
    sum$total.included.pct <- round(sum$total.included / total.sample, 4)
    
    parties.to.drop <- find.groups.to.drop(main.summary.data, "Party")
    
    sum$party.missing <- main.summary.data %>% 
      filter(Party %in% c(cats.to.drop, parties.to.drop)) %>%
      summarise(n = sum(n.weighted)) %>% 
      pull(n)
    
    sum$party.missing.pct <- round(sum$party.missing / total.sample, 4)
    
    groups.to.drop <- find.groups.to.drop(main.summary.data, "Group")
    
    sum$group.missing <- main.summary.data %>% 
      filter(Group %in% c(cats.to.drop, groups.to.drop)) %>%
      summarise(n = sum(n.weighted)) %>% 
      pull(n)
    
    sum$group.missing.pct <- round(sum$group.missing / total.sample, 4)
    
    sum
  })
}

get.group.size.summary <- function(res, group.to.use = NULL) {
  # Take a DF, and binds each row to a single row
  bind.to.sigle.row <- function(df) {
    df <- split(df, seq(nrow(df)))
    bind_cols(df)
  }
  
  # Add in group sizes for 3 largest groups for each country,
  #  as well as breakdowns for each Party/Main Group combo
  get.grp.size.row <- function(country.data) {
    if (is.null(group.to.use))
      group.basis <- country.data$Summary$general$`Group Basis`
    else
      group.basis <- group.to.use
    
    if (is.na(group.basis) || is.null(group.basis))
      return(NULL)
    
    summary.data <- config.summary.data(country.data[[group.basis]], drop.cats = T, weighted = T)
    
    if (! is.data.frame(summary.data))
      return(NULL)
    
    gs.row <- country.data$Summary$general %>%
      select(Country, `Data Source`, Year) %>%
      mutate(`Group Basis` = group.basis)
    
    gs <- summary.data %>% 
      group_by(Group) %>% 
      summarise(n = sum(n)) %>% 
      filter(n > 0) %>%
      slice_max(n, n=5, with_ties = F) %>%
      arrange(desc(n))
    
    main.groups <- gs$Group
    
    summary.data <- summary.data %>% mutate(Group = fct_relevel(Group, as.character(main.groups)))
    
    for (row in 1:summary.group.size) {
      sum.row <- gs[row, ]
      gs.row <- suppressMessages(bind_cols(gs.row, sum.row))
    }    
    
    #gs.row <- suppressMessages(bind_cols(gs.row, bind.to.sigle.row(gs)))
    
    party.group.sizes <- summary.data %>% 
      filter(Group %in% main.groups) %>% 
      arrange(Group) %>%
      pivot_wider(names_from = Group, values_from = n) %>%
      rowwise() %>% 
      mutate(Total = sum(c_across(-Party))) %>%
      select(Party, Total, everything()) %>%
      arrange(desc(Total))
    
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
    
    #gs.row <- suppressMessages(bind_cols(gs.row, bind.to.sigle.row(party.group.sizes)))
    
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
  }
  
  furrr::future_map_dfr(res, get.grp.size.row)
}

get.max.parties <- function(group.sizes) {
  length(names(group.sizes)[str_detect(names(group.sizes), "^Party.Grp")])
}
