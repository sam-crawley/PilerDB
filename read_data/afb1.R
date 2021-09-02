data.spec <- list(
  file.name = "Divided/data/afrobarom/merged_r1_data.sav",
  file.type = 'sav',
  file.encoding = "latin1",
  skip.countries = list(
    no_group = c("Botswana", "Lesotho")
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "party",
    "Language" = "language",
    "Religion" = NA,
    "Ethnicity" = NA,
    "Weight" = "combinwt",
    "Country" = "country",
    "Year" = NA
  ),
  fixups = function(data) {
    data %>% mutate(Year = 2001)
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Neutral/No party", "Don't Know", "Refused", "Missing Data")
  ),
  Language = list(
    "Missing" = c("Missing data", "Refused to answer"),
    "Other" = c("Other")
  )
)
