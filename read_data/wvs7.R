data.spec <- list(
  file.name = "Divided/data/WVS/W7/WVS_Cross-National_Wave_7_stata_v2_0.dta",
  file.type = 'dta',
  skip.countries = c("China", "Egypt", "Vietnam", "Jordan", "Singapore"),
  country.format = 'iso3c',
  field.def = c(
    "Party" = "Q223",
    "Language" = "Q272",
    "Religion" = "Q289",
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
    "Other" = c("Other", "Other European", "Other Chinese dialects", "Other local; aboriginal; tribal, community"),
    "Missing" = c("Missing; Not available", "Not asked", "Don\ub4t know", "No answer")
  ),
  Religion = list(
    "Other" = c("Other", "Other Christian (Pentecostal/Free church/Jehova...)", "Do not belong to a denomination"),
    "Missing" = c("No answer/refused", "Don't know", "Other missing; Multiple answers Mail (EVS)")
  ),
  Ethnicity = list(
    "Other" = c("Other, Non-Hispanic", "Two plus, non-Hispanic", "Cross breed", "Other Asian", "Other Africans", "Others"),
    "Missing" = c("Missing; Not specified", "Not asked", "Don\ub4t know", "No answer")
  )
)
