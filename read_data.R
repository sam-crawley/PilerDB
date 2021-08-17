library(tidyverse)
library(haven)
library(countrycode)

# Data Spec fields
# * file.name = file name to load
# * file.type = either sav or dta
# * file.encoding = encoding to use when reading the file, defaults to UTF-8
# * field.def = vector of fields to use for the analysis, with names being the column names
#               (names should be ones from field.names)
#               For the main vars, if the field is missing from the dataset, it should be set
#               to NA
# * country.format = the original format of the country (see countrycode::codelist)
# * country.custom = names vector to be passed to custom_match parameter of countrycode()
# * skip.countries = list of countries to skip completely (using full country.name text)
# * fixups = a function to apply some custom fixups to the data before returning it

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)
allowed.field.names <- c(main.vars, "Country", "Year", "Weight")

read.div.data <- function(data.spec, raw = F) {
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
    ) %>%
    filter(! Country %in% data.spec$skip.countries)
  
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

# Do some checks on the data to find out if all countries included have enough data
check.data <- function(data, cat.defs) {
  data <- process.data(data, cat.defs)
  
  # Check for countries without Party data
  res <- data %>% 
    group_by(Country) %>% 
    summarise(party.resp.count = sum(Party != "Missing")) %>%
    filter(party.resp.count == 0)
  
  if (nrow(res) > 0)
    stop("The following countries have no Party data (they should be added to skip.countries): ", paste(res %>% pull(Country), collapse = ", "))
  
  missing.all <- keep(unique(data$Country), function(c) {
    combos <- data %>% filter(Country == c) %>% count(Religion, Ethnicity, Language)
    
    nrow(combos) == 1
  })
  
  if (length(missing.all) > 0)
    stop("The following counrties do not appear to have any usable group data: ", paste(missing.all, collapse = ", "))
}

### Some util functions

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