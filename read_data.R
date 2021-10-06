library(tidyverse)
library(haven)
library(countrycode)
library(here)

# Data Spec fields
# * file.name = file name to load
# * file.type = either sav or dta
# * file.encoding = encoding to use when reading the file, defaults to UTF-8
# * field.def = vector of fields to use for the analysis, with names being the column names
#               (names should be ones from field.names)
#               For the main vars, if the field is missing from the dataset, it should be set
#               to NA
# * wave_var = for multiwave datasets, this variable in the data indicates the wave/module
#             Data is split based on this variable, and the value of the variable is appended
#             to the data source ID (meaning each wave appears as a different data source)
# * split_by_year = logical. If true, splits out data by the value of the Year variable
#                   Some datasets (CSES) contain multiple surveys for the same country,
#                   so this separates them out.
# * country.format = the original format of the country (see countrycode::codelist)
# * country.custom = names vector to be passed to custom_match parameter of countrycode()
# * skip.countries = list of countries to skip completely (using full country.name text)
# * fixups = a function to apply some custom fixups to the data before returning it
# * pre_fixups = a function to apply custom fixups to the data *before* other processing is done

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)
allowed.field.names <- c(main.vars, "Country", "Year", "Weight")

read.div.data <- function(data.spec, raw = F, ignore.skip.countries = F) {
  if (! all(names(data.spec$field.def) %in% allowed.field.names))
    stop("field.def contains invalid field names")
  
  rename.spec <- data.spec$field.def[! is.na(data.spec$field.def)]
  
  if (data.spec$file.type == "dta")
    read_func <- read_dta
  else if (data.spec$file.type == "sav")
    read_func <- read_sav
  else
    stop("Unknown file type")
  
  encoding <- if_else(is.null(data.spec$file.encoding), "UTF-8", data.spec$file.encoding)
  
  data <- read_func(data.spec$file.name, encoding = encoding)
  
  if (raw)
    return(data)
  
  prefixup.func <- data.spec$pre_fixups
  if (! is.null(prefixup.func))
    data <- prefixup.func(data)
  
  data <- data %>%
    rename(all_of(rename.spec))
  
  if (is.labelled(data$Country))
    data$Country <- as.character(haven::as_factor(data$Country))
  
  data <- data %>%
    mutate(
      Country.orig = Country,
      Country = countrycode(Country, origin = data.spec$country.format, destination = 'country.name', custom_match = data.spec$country.custom),
    )
  
  if (! ignore.skip.countries)
    data <- data %>%
      filter(! Country %in% unlist(data.spec$skip.countries))
  
  main.var.def <- data.spec$field.def[main.vars]
  
  available.vars <- names(main.var.def[! is.na(main.var.def)])
  missing.vars <- names(main.var.def[is.na(main.var.def)])
  
  for (v in missing.vars) {
    data[[v]] <- factor("Missing")
  }
  
  data <- data %>%
    mutate(across(all_of(available.vars), haven::as_factor)) %>%
    mutate(across(all_of(available.vars), ~fct_explicit_na(.x, na_level = "Missing"))) %>%
    arrange(Country)
  
  fixup.func <- data.spec$fixups
  
  if (! is.null(fixup.func))
    data <- fixup.func(data)
  
  return(data)
}

load.data.by.id <- function(id, process = T, ignore.skip.countries = F) {
  file <- paste0(here("Divided/read_data"), "/", tolower(id), ".R")
  
  if (! file.exists(file))
    stop("Cannot find data.def file: ", file)
  
  e <- new.env()
  
  source(file, local = e)
  
  data <- read.div.data(e$data.spec, ignore.skip.countries = ignore.skip.countries)
  
  if (process)
    data <- process.data(data, e$cat.defs)
  
  data
}

check.data.by.id <- function(id) {
  data <- load.data.by.id(id, ignore.skip.countries = T)
  
  file <- paste0(here("Divided/read_data/"), "/", tolower(id), ".R")
  
  e <- new.env()
  
  source(file, local = e)  
  
  check.data(data, e$data.spec$skip.countries)
}

get.data.def.list <- function() {
  list.files(here("Divided/read_data"), pattern="*.R$", full.names=T)
}

get.data.def.id <- function(filename) {
  toupper( str_match(filename, "/(\\w+?).R$")[,2] )
}

# Check the skip.countries list to see if it contains the correct list of countries
# Note, we assume countries should only be excluded if they have no (non-missing) parties and/or groups
#  (This doesn't necessarily mean we can calculate Gallagher, etc)
# TODO: check low_n skipped countries
check.data <- function(data, skip.countries) {
  # Check for countries without Party data
  res <- data %>% 
    group_by(Country) %>% 
    summarise(party.resp.count = sum(! Party %in% c("Missing", "Other"))) %>%
    filter(party.resp.count == 0)
  
  no_party_list = res %>% pull(Country)
  
  diff.list <- function(a, b) {
    setdiff(a, b) %>% discard(~ .x %in% global.country.skip)
  }
  
  no_country_additional <- diff.list(skip.countries[['no_party']], no_party_list)
  no_country_missing <- diff.list(no_party_list, skip.countries[['no_party']])
  if (length(no_country_additional > 0)) {
    cat("The following countries are on skip.countries no_party list, but should not be: ", paste(no_country_additional, collapse = ", "), "\n")
  }
  if (length(no_country_missing > 0)) {
    cat("The following countries are missing from skip.countries no_party list: ", paste(no_country_missing, collapse = ", "), "\n")
  }      
  
  no_group_list <- keep(unique(data$Country) %>% discard(~ .x %in% no_party_list), function(c) {
    d <- data %>% filter(Country == c) 
    
    groups <- map(group.names, function(g) {
      res <- fct_count(d[[g]]) %>%
        filter(! f %in% c("Missing", "Other") & n != 0)
      
      if (nrow(res) <= 1)
        return (NULL)
      else
        return (g)
    })
    
    length(unlist(groups)) == 0
  })
  
  no_group_additional <- diff.list(skip.countries[['no_group']], no_group_list)
  no_group_missing <- diff.list(no_group_list, skip.countries[['no_group']])
  if (length(no_group_additional > 0)) {
    cat("The following countries are on skip.countries no_group list, but should not be: ", paste(no_group_additional, collapse = ", "), "\n")
  }
  if (length(no_group_missing > 0)) {
    cat("The following countries are missing from skip.countries no_group list: ", paste(no_group_missing, collapse = ", "), "\n")
  }
}

# Check all datasets to see if the list of skip countries matches what we think should be skipped
check.all.datasets <- function() {
  data.defs <- get.data.def.list()
  
  for (data.def in data.defs) {
    id <- get.data.def.id(data.def)
    
    cat("Checking", id, "\n")
    
    check.data.by.id(id)
  }
}

### Util functions that can be called when loading data (i.e. as part of 'fixups')

# Coalesce a list of variables into a single column
#  (Needed because ESS has separate columns per country)
coalese.vars <- function(data, cols, new_var)  {
  data <- data %>% 
    mutate(across(all_of(cols), ~haven::as_factor(.x, levels = "label")))
  
  data[[new_var]] <- NA_character_
  
  for (c in cols) {
    data[[new_var]] <- coalesce(data[[new_var]], data[[c]])  
  }
  
  data
}