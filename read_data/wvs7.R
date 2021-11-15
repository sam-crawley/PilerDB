data.spec <- list(
  file.name = "Divided/datasets/WVS/W7/WVS_Cross-National_Wave_7_stata_v2_0.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were a national election tomorrow, for which party on this list would you vote?",
    "Religion" = "Do you belong to a religious denomination? (If yes) Which one?",
    "Ethnicity" = "Ethnic group [exact question wording not supplied]",
    "Language" = "What language do you normally speak at home?"
  ),    
  skip.countries = list(
    no_party = c("China", "Vietnam", "Jordan", "Singapore")
  ),
  country.format = 'iso3c',
  field.def = c(
    "Party" = "Q223",
    "Language" = "Q272",
    "Religion" = "Q289CS9",
    "Ethnicity" = "Q290",
    "Country" = "B_COUNTRY_ALPHA",
    "Year" = "A_YEAR",
    "Weight" = "W_WEIGHT"
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
    "Missing" = c("Not applicable", "No answer", "Don\ub4t know", "No right to vote", "I would not vote", "Missing, Not available",
                                "I would cast a blank ballot; White vote", "None", "Null vote", "Not asked"),
    "Other" = c("Other", "Independent candidate")
  ),
  Language = list(
    "Other" = c("Other", "Other European"),
    "Missing" = c("Missing; Not available", "Not asked", "Don\ub4t know", "No answer")
  ),
  Religion = list(
    "Other" = c("Non-religious", "Agnostic", "Atheist", "Other; nfd"),
    "Missing" = c("No answer", "Dont know", "Other missing", "Item not included")
  ),
  Ethnicity = list(
    "Other" = c("Other, Non-Hispanic", "Two plus, non-Hispanic", "Others"),
    "Missing" = c("Missing; Not specified", "Not asked", "Don\ub4t know", "No answer")
  )
)
