data.spec <- list(
  file.name = "Divided/datasets/WVS/W4/WV4_Data_stata_v20201117.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were a national election tomorrow, for which party on this list would you vote?",
    "Religion" = "Do you belong to a religious denomination? (If yes) Which one?",
    "Ethnicity" = "Ethnic group [exact question wording not supplied]",
    "Language" = "What language do you normally speak at home?"
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "V220",
    "Language" = "V219",
    "Religion" = "v184b",
    "Ethnicity" = "V242",
    "Country" = "V2",
    "Year" = "V246",
    "Weight" = "V245"
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
    "Missing" = c("No answer", "No right to vote", "I would not vote",
                  "I would cast a blank ballot; White vote", "None", "Null vote", "Not asked"),
    "Other" = c("Other", "Independent candidate")
  ),
  Language = list(
    "Missing" = c("Not asked", "No answer"),
    "Other" = c("Other", "Other European")
  ),
  Religion = list(
    "Missing" = c("No answer"),
    "Other" = c("Other; nfd"),
    "No religion" = c("Noneligious")
  ),
  Ethnicity = list(
    "Missing" = c("Missing; Not specified", "Not asked", "No answer", "No Religion No caste"),
    "Other" = c("Other, Non-Hispanic", "Others")
  )
)