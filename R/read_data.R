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
# * country.dict = country dict (passed to custom_dict) to use
# * country.custom = names vector to be passed to custom_match parameter of countrycode()
# * fixups = a function to apply some custom fixups to the data before returning it
# * pre_fixups = a function to apply custom fixups to the data *before* other processing is done

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)
allowed.field.names <- c(main.vars, "Country", "Year", "Weight")

# Directory where the definition R files can be found
data.def.dir <- "data_defs"

read.div.data <- function(data.spec, raw = F) {
  if (! all(names(data.spec$field.def) %in% allowed.field.names))
    stop("field.def contains invalid field names")
  
  if (! file.exists(data.spec$file.name))
    stop("Cannot find file ", data.spec$file.name)
  
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
    data$Country <- str_trim(as.character(haven::as_factor(data$Country)))
  
  data <- data %>%
    mutate(
      Country.orig = Country,
      Country = countrycode(Country, 
                            origin = data.spec$country.format, 
                            destination = 'country.name', 
                            custom_match = data.spec$country.custom,
                            custom_dict = data.spec$country.dict
      )
    )
  
  main.var.def <- data.spec$field.def[main.vars]
  
  available.vars <- names(main.var.def[! is.na(main.var.def)])
  missing.vars <- names(main.var.def[is.na(main.var.def)])
  
  for (v in missing.vars) {
    data[[v]] <- factor("Missing")
  }
  
  data <- data %>%
    mutate(across(all_of(available.vars), haven::as_factor)) %>%
    mutate(across(all_of(available.vars), ~fct_explicit_na(.x, na_level = "Missing"))) %>%
    mutate(Weight = as.numeric(Weight)) %>%
    arrange(Country)
  
  fixup.func <- data.spec$fixups
  
  if (! is.null(fixup.func))
    data <- fixup.func(data)
  
  return(data)
}

load.data.by.id <- function(id, process = T) {
  file <- paste0(here("Divided/read_data"), "/", tolower(id), ".R")
  
  if (! file.exists(file))
    stop("Cannot find data.def file: ", file)
  
  e <- new.env()
  
  source(file, local = e, encoding = "UTF-8")
  
  data <- read.div.data(e$data.spec)
  
  if (process)
    data <- process.data(data, e$cat.defs)
  
  data
}

get.data.def.list <- function() {
  if (! dir.exists(data.def.dir))
    stop("Cannot find data def dir ", data.def.dir)
  
  list.files(here(data.def.dir), pattern="*.R$", full.names=T)
}

get.data.def.id <- function(filename) {
  toupper( str_match(filename, "/(\\w+?).R$")[,2] )
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

get.es.countries <- function() {
  c <- tibble(spanish = countrycode::codelist$cldr.name.es,
             country.name = countrycode::codelist$cldr.name.en) %>%
    bind_rows(c(
      'spanish' = 'Rep. Dominicana',
      'country.name' = 'Dominican Republic'
    ))
  
  c.no.accent <- tibble(spanish = stringi::stri_trans_general(countrycode::codelist$cldr.name.es, id = "Latin-ASCII"),
                        country.name = countrycode::codelist$cldr.name.en)
  
  bind_rows(c, c.no.accent) %>% distinct(spanish, .keep_all = T)
}

country.dict.es <- get.es.countries()