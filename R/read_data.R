# Functions to read the files for the datasets.

group.names <- c("Language", "Religion", "Ethnicity")
main.vars <- c("Party", group.names)
allowed.field.names <- c(main.vars, "Country", "Year", "Weight")
allowed.party.qtypes <- c("Closest", "PartyVote", "PresPartyVote", "PresCand")

read.div.data <- function(data.spec, data.def.file, raw = F, datasets.dir = NULL) {
  if (! all(names(data.spec$field.def) %in% allowed.field.names))
    stop("field.def contains invalid field names")
  
  if (! data.spec$party.question.type %in% allowed.party.qtypes)
    stop("party.question.type not defined or invalid")

  if (is.null(datasets.dir))
    datasets.dir <- get.datasets.dir()
  
  file.path <- get.dataset.file.path(data.def.file = data.def.file, datasets.dir = datasets.dir, data.spec = data.spec)
  
  if (! file.exists(file.path))
    stop("Cannot find file ", file.path)
  
  rename.spec <- data.spec$field.def[! is.na(data.spec$field.def)]
  
  if (data.spec$file.type == "dta")
    read_func <- haven::read_dta
  else if (data.spec$file.type == "sav")
    read_func <- haven::read_sav
  else
    stop("Unknown file type")
  
  encoding <- ifelse(is.null(data.spec$file.encoding), "UTF-8", data.spec$file.encoding)
  
  data <- read_func(file.path, encoding = encoding)
  
  if (raw)
    return(data)
  
  prefixup.func <- data.spec$pre_fixups
  if (! is.null(prefixup.func))
    data <- prefixup.func(data)
  
  data <- data %>%
    rename(all_of(rename.spec))
  
  if (haven::is.labelled(data$Country))
    data$Country <- str_trim(as.character(haven::as_factor(data$Country)))
  
  data <- data %>%
    mutate(
      Country.orig = Country,
      Country = countrycode::countrycode(Country, 
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
    mutate(across(all_of(available.vars), ~fct_na_value_to_level(.x, level = "Missing"))) %>%
    mutate(Weight = as.numeric(Weight)) %>%
    arrange(Country)
  
  fixup.func <- data.spec$fixups
  
  if (! is.null(fixup.func))
    data <- fixup.func(data)
  
  return(data)
}

# Get directory where the definition R files can be found
get.data.def.dir <- function() {
  system.file("data_defs", package=packageName())
}

# Directory where the survey datasets are stored (by default)
get.datasets.dir <- function() {
  here::here("datasets")
}

#' @export
load.data.by.id <- function(id, raw = F, process = T, datasets.dir = NULL) {
  process <- if_else(raw, F, process)
  
  file <- paste0(get.data.def.dir(), "/", tolower(id), ".R")
  
  if (! file.exists(file))
    stop("Cannot find data.def file: ", file)
  
  e <- new.env()
    
  source(file, local = e, encoding = "UTF-8")
    
  data <- read.div.data(e$data.spec, file, raw = raw, datasets.dir = datasets.dir)

  if (process)
    data <- process.data(data, e$cat.defs)
  
  data
}

get.data.def.list <- function() {
  data.def.dir <- get.data.def.dir()
  
  if (! dir.exists(data.def.dir))
    stop("Cannot find data def dir ", data.def.dir)
  
  list.files(data.def.dir, pattern="*.R$", full.names=T)
}

get.data.def.id <- function(filename) {
  toupper( str_match(filename, "/(\\w+?).R$")[,2] )
}

get.dataset.file.path <- function(data.def.file, datasets.dir, data.spec) {
  def.id <- get.data.def.id(data.def.file)
  def.prefix <- str_remove(def.id, "\\d+.?$")
  
  if (def.id == def.prefix)
    def.prefix <- NULL
  
  file.path <- paste(datasets.dir, def.prefix, def.id, data.spec$file.name, sep = "/")
}

check.dataset.files <- function() {
  walk(get.data.def.list(), function(def.file) {
    e <- new.env()
    
    source(def.file, local = e, encoding = "UTF-8")
    
    file.path <- get.dataset.file.path(data.def.file = def.file, datasets.dir = default.datasets.dir, data.spec = e$data.spec)
    
    if (! file.exists(file.path)) {
      cat("Cannot find file", file.path, "\n")
    }
  })
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
    dplyr::bind_rows(c(
      'spanish' = 'Rep. Dominicana',
      'country.name' = 'Dominican Republic'
    ))
  
  c.no.accent <- tibble(spanish = stringi::stri_trans_general(countrycode::codelist$cldr.name.es, id = "Latin-ASCII"),
                        country.name = countrycode::codelist$cldr.name.en)
  
  dplyr::bind_rows(c, c.no.accent) %>% dplyr::distinct(spanish, .keep_all = T)
}

country.dict.es <- get.es.countries()