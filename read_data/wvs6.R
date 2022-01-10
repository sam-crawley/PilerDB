data.spec <- list(
  file.name = "Divided/datasets/WVS/W6/WV6_Data_stata_v20201117.dta",
  file.type = 'dta',
  question.text = c(
    "Party" = "If there were a national election tomorrow, for which party on this list would you vote?",
    "Religion" = "Do you belong to a religious denomination? (If yes) Which one?",
    "Ethnicity" = "Ethnic group [exact question wording not supplied]",
    "Language" = "What language do you normally speak at home?"
  ),  
  skip.countries = list(
    no_party = c("Belarus", "China", "Kuwait", "Lebanon", "Qatar", "Trinidad & Tobago", "Uzbekistan")
  ),
  country.format = 'iso3c',
  field.def = c(
    "Party" = "V228",
    "Language" = "V247",
    "Religion" = "V144",
    "Ethnicity" = "V254",
    "Country" = "B_COUNTRY_ALPHA",
    "Year" = "V262",
    "Weight" = "V258"
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
    "Missing" = c("Not applicable", "No answer", "No right to vote", "I would not vote",
                  "I would cast a blank ballot; White vote", "None", "Null vote", "Not asked"),
    "Other" = c("Other", "Independent candidate")
  ),
  Language = list(
    "Other" = c("Other", "Other Oceania")
  ),
  Religion = list(
    "Other" = c("Other; nfd", "Non classified christian movements; nfd"),
    "No Religion" = c("Noneligious")
  ),
  Ethnicity = list(
    "Other" = c("Other, Non-Hispanic", "Two plus, non-Hispanic")
  )
)