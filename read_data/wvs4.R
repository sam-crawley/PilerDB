data.spec <- list(
  file.name = "Divided/data/WVS/W4/WV4_Data_stata_v20201117.dta",
  file.type = 'dta',
  skip.countries = c('China', 'Saudi Arabia', 'Singapore', 'South Korea', 'Vietnam'),
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
    "Other" = c("Other", "Other local; aboriginal; tribal, community", "Other European")
  ),
  Religion = list(
    "Missing" = c("No answer"),
    "Other" = c("Noneligious")
  ),
  Ethnicity = list(
    "Missing" = c("Missing; Not specified", "Not asked", "No answer"),
    "Other" = c("Other, Non-Hispanic")
  )
)