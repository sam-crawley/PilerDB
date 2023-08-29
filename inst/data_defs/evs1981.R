data.spec <- list(
  file.name = "ZA4438_v3-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Do you consider yourself to be close or not to any particular party? [If yes] To which party?",
    "Religion" = "Do you belong to a religious denomination? [If yes] Which one?"
  ),
  party.question.type = "Closest",
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom"
  ),
  field.def = c(
    "Party" = "v532",
    "Language" = NA,
    "Religion" = "v271",
    "Ethnicity" = NA,
    "Weight" = "weight_g",
    "Country" = "c_abrv",
    "Year" = "year"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\s*[\\w-]+:\\s*")
    }
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Missing", "None", "unknown"),
    "Other" = c("Other", "Independent")
  ),
  Religion = list(
    "Missing" = c("Missing"),
    "Other" = c("OTHER"),
    "No Religion" = c("NONE")
  )
)
