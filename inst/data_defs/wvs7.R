data.spec <- list(
  file.name = "WVS_Cross-National_Wave_7_stata_v6_0.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were a national election tomorrow, for which party on this list would you vote?",
    "Religion" = "Do you belong to a religious denomination? (If yes) Which one?",
    "Ethnicity" = "Ethnic group [exact question wording not supplied]",
    "Language" = "What language do you normally speak at home?"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso3c',
  country.custom = c(
    "NIR" = "United Kingdom"
  ),
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
    "Missing" = c("Not applicable", "No answer", "Don't know", "No right to vote", "I would not vote", "Missing, Not available",
                                "I would cast a blank ballot; White vote", "None", "Null vote", "-4"),
    "Other" = c("Other", "Independent candidate")
  ),
  Language = list(
    "Other" = c("Other", "Other European", "Other Chinese dialects", "Other local; aboriginal; tribal, community"),
    "Missing" = c("Missing; Not available", "Don't know", "No answer", "-4")
  ),
  Religion = list(
    "Other" = c("Other; nfd", "Ethnic religions\xc2\xa0excluding some in separate categories; nfd",
                "African traditional religions Ethnic, nfd"),
    "Missing" = c("No answer", "Dont know", "Other missing", "Item not included"),
    "No Religion" = c("Non-religious", "Agnostic", "Atheist", "Secular /Nonreligious /Agnostic/Atheist; nfd")
  ),
  Ethnicity = list(
    "Other" = c("Other, Non-Hispanic", "Two plus, non-Hispanic", "Others", "Not pertaining to Indigenous groups",
                "Indigenous with no further detail", "Other Africans/Negro Black", "Other Asian", "Other (write in)",
                "Other Africans", "Migrant of other origin", "Black-Other", "Any other White background, please describe",
                "Any other Asian background, please describe", "Other ethnic group",
                "Any other Black / African / Caribbean background, please describe"),
    "Missing" = c("Missing; Not specified", "Not asked", "Don't know", "No answer", "Not applicable")
  )
)
