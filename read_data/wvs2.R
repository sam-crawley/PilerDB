data.spec <- list(
  file.name = "Divided/data/WVS/W2/WV2_Data_Stata_v20180912.dta",
  file.type = 'dta',
  skip.countries = list(
    no_party = c('Argentina', 'China', 'Russia', 'South Korea'),
    no_group = c("Belarus", "Japan", "Poland", "Spain", "Turkey")
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "V351",
    "Language" = NA,
    "Religion" = "V144",
    "Ethnicity" = "V369",
    "Country" = "V2",
    "Year" = "V377",
    "Weight" = "V376"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\w+:\\s*")
    }
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No answer", "I would not vote", "Missing, Not available", "Dont know",
                  "I would cast a blank ballot; White vote", "None", "Not asked", "Undocumented"),
    "Other" = c("Other", "Independent")
  ),
  Religion = list(
    "Missing" = c("Missing; Not asked by the interviewer", "No answer", "Dont know"),
    "Other" = c("None")
  ),
  Ethnicity = list(
    "Missing" = c("Missing; Not specified", "Not asked", "No answer", "Dont know"),
    "Other" = c("Other", "Others")
  )
)