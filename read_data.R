library(tidyverse)
library(haven)
library(countrycode)

# Data Spec fields
# * file.name = file name to load
# * file.type = either sav or dta
# * field.def = vector of fields to use for the analysis, with names being the column names
#               (names should be ones from field.names)
#               For the main vars, if the field is missing from the dataset, it should be set
#               to NA
# * country.format = the original format of the country (see countrycode::codelist)
# * skip.countries = list of countries to skip completely (using full country.name text)
# * fixups = a function to apply some custom fixups to the data before returning it

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)
allowed.field.names <- c(main.vars, "Country", "Year", "Weight")

read.div.data <- function(data.spec) {
  if (! all(names(data.spec$field.def) %in% allowed.field.names))
    stop("field.def contains invalid field names")
  
  rename.spec <- data.spec$field.def[! is.na(data.spec$field.def)]
  
  if (data.spec$file.type == "dta")
    read_func <- read_dta
  else if (data.spec$file.type == "sav")
    read_func <- read_sav
  else
    stop("Unknown file type")
  
  data <- read_func(data.spec$file.name, encoding = "UTF-8") %>%
    rename(all_of(rename.spec))
  
  if (is.labelled(data$Country))
    data$Country <- as.character(haven::as_factor(data$Country))
  
  data <- data %>%
    mutate(Country = countrycode(Country, origin = data.spec$country.format, destination = 'country.name')) %>%
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
  data <- fixup.func(data)
  
  # Check for countries without Party data
  res <- data %>% 
    group_by(Country) %>% 
    summarise(party.resp.count = sum(Party != "Missing")) %>%
    filter(party.resp.count == 0)
  
  if (nrow(res) > 1)
    stop("The following countries have no Party data (they should be added to skip.countries): ", paste(res %>% pull(Country), collapse = ", "))
  
  return(data)
}