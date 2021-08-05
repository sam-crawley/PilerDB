data.spec <- list(
  file.name = "Divided/data/afrobarom/merged-round-5-data-34-countries-2011-2013-last-update-july-2015.sav",
  file.type = 'sav',
  skip.countries = c('Eswatini'),
  country.format = 'country.name',
  field.def = c(
    "Party" = "Q89B",
    "Language" = "Q2",
    "Religion" = "Q98A",
    "Ethnicity" = "Q84",
    "Weight" = "withinwt",
    "Country" = "COUNTRY",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = format(DATEINTR, format = "%Y"))
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "Don't know", "Refused to answer", "Not Applicable"),
    "Other" = c("Others")
  ),
  Language = list(
    "Missing" = c("Missing", "Don't know"),
    "Other" = c("Others")
  ),
  Religion = list(
    "Missing" = c("Missing", "Don't know", "Refused"),
    "Other" = c("Other", "None")
  ),
  Ethnicity = list(
    "Other" = c("Related to regional origin (badio/sampadjudo)",
                "Related to political-partisan affiliation",
                "Related to age",
                "Related to gender",
                "Related to race",
                "Others"
    ),
    "Missing" = c("Missing", "Don't know", "Refused")
  )
)
