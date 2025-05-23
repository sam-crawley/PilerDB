data.spec <- list(
  file.name = "WV2_Data_Stata_v20180912.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were a general election tomorrow, which party would you vote for?",
    "Religion" = "Do you belong to a religious denomination? (If yes) Which one?",
    "Ethnicity" = "Ethnic group [exact question wording not supplied]"
  ),
  party.question.type = "PartyVote",
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
    "Missing" = c("Missing; Not asked by the interviewer", "No answer", "Dont know", "Not asked in survey"),
    "No Religion" = c("None")
  ),
  Ethnicity = list(
    "Missing" = c("Missing; Not specified", "Not asked", "No answer", "Dont know"),
    "Other" = c("Other", "Others")
  )
)
