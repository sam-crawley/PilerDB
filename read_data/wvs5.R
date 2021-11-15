data.spec <- list(
  file.name = "Divided/datasets/WVS/W5/WV5_Data_Stata_v20180912.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were a national election tomorrow, for which party on this list would you vote?",
    "Religion" = "Do you belong to a religious denomination? (If yes) Which one?",
    "Ethnicity" = "Ethnic group [exact question wording not supplied]",
    "Language" = "What language do you normally speak at home?"
  ),  
  skip.countries = list(
    no_party = c('France', 'Italy', 'Jordan', 'Malaysia', 'Netherlands', 'Russia', 'Rwanda', 'Trinidad & Tobago', 'United Kingdom', 'Vietnam', 'China')
  ),
  country.format = 'country.name',
  field.def = c(
    "Party" = "V231",
    "Language" = "V222",
    "Religion" = "V185",
    "Ethnicity" = "V256",
    "Country" = "V2",
    "Year" = "V260",
    "Weight" = "V259"
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
    "Missing" = c("Not applicable", "No answer", "No right to vote", "I would not vote", "Dont know", "Missing, Not available",
                  "I would cast a blank ballot; White vote", "None", "Null vote", "Not asked", "Vote for persons, not parties"),
    "Other" = c("Independent", "Independiente (Independents)", "Candidatos Independientes (no partidistas)", "Independent parties",
                "Indepedents")
  ),
  Language = list(
    "Missing" = c("Not asked", "No answer", "Dont know"),
    "Other" = c("Other", "Other South Asian", "Other African")
  ),
  Religion = list(
    "Missing" = c("Missing; Not asked by the interviewer", "No answer", "Dont know"),
    "Other" = c("Other", "None")
  ),
  Ethnicity = list(
    "Missing" = c("Missing; Not specified", "Not asked", "No answer", "Dont know"),
    "Other" = c("Other", "Mixed races")
  )
)