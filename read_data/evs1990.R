data.spec <- list(
  file.name = "Divided/datasets/evs/1990/ZA4460_v3-0-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "If there was a general election tomorrow, which party would you vote for?",
    "Religion" = "Do you belong to a religious denomination? [If yes] Which one?"
  ),  
  skip.countries = list(
    no_party = c("Estonia", "Latvia", "Lithuania", "Malta")
  ),
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "GB-NIR" = "United Kingdom"
  ),
  field.def = c(
    "Party" = "q674a",
    "Language" = NA,
    "Religion" = "q333b",
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
    "Missing" = c("Missing", "I would not vote", "I would cast a blank ballot", "No right to vote", "None", "undocumented", "I would not go to election",
                  "unknown", "no vote"),
    "Other" = c("Other", "other party", "other", "other party/movement", "Other party", "Other/independent", "Other parties",
                "Right-only general formulation", "Center", "Left-only general formulation", "Well governing, just, wise party",
                "Depends on candidates, on programs, will vote for people", "Non-party people", "None, will not vote for any party, not for any other",
                "Other answers", "Independent")
  ),
  Religion = list(
    "Missing" = c("Missing"),
    "Other" = c("Other")
  )
)
