data.spec <- list(
  file.name = "Divided/data/WVS/W3/WV3_Data_Stata_v20180912.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were a national election tomorrow, for which party on this list would you vote?",
    "Religion" = "Do you belong to a religious denomination? (If yes) Which one?",
    "Ethnicity" = "Ethnic group [exact question wording not supplied]",
    "Language" = "What language do you normally speak at home?"
  ),
  skip.countries = list(
    no_party = c('China', 'Nigeria', 'Pakistan', 'South Korea'),
    no_group = c("Argentina", "Croatia", "Japan", "Poland", "Slovenia", "Turkey", "United Kingdom"),
    low_n = c("Montenegro")
  ),
  country.format = 'country.name',
  country.custom = c("SrpSka Republic" = "SrpSka Republic"),
  field.def = c(
    "Party" = "V210",
    "Language" = "V209",
    "Religion" = "V179",
    "Ethnicity" = "V233",
    "Country" = "V2",
    "Year" = "V238",
    "Weight" = "V236"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\w+:\\s*")
    }
    
    # Combine SrpSka Republic data with Bosnia
    data <- data %>% mutate(Country = if_else(Country == "SrpSka Republic", "Bosnia & Herzegovina", Country))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("No answer", "No right to vote", "I would not vote", "Missing, Not available", "Dont know",
                  "I would cast a blank ballot; White vote", "None", "Not asked"),
    "Other" = c("Other", "Other party", "Other 1", "Other 2", "Independent", "Candidatos Independientes (no partidistas)",
                "Independent Candidate")
  ),
  Language = list(
    "Missing" = c("Not asked", "No answer", "Dont know"),
    "Other" = c("Other", "Other European", "Other Indian Language", "Other Philipinan language", "Other Yugoslavian", "More than one")
  ),
  Religion = list(
    "Missing" = c("Not asked", "NA", "DK"),
    "Other" = c("Other", "doesnt follow rules", "No, not a member")
  ),
  Ethnicity = list(
    "Missing" = c("Missing; Not specified", "Not asked", "No answer", "Dont know"),
    "Other" = c("Other, Non-Hispanic", "Undocumented 1", "Other ethnic group")
  )
)