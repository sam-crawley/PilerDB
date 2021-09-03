data.spec <- list(
  file.name = "Divided/data/WVS/W6/WV6_Data_stata_v20201117.dta",
  file.type = 'dta',
  skip.countries = list(
    no_party = c("Belarus", "China", "Kuwait", "Lebanon", "Qatar", "Trinidad & Tobago", "Uzbekistan"),
    no_group = c("Argentina", "Armenia", "Egypt", "Japan", "Jordan", "Kuwait", "Palestinian Territories", "Poland", "Slovenia", "Spain", "Tunisia", "Yemen"),
    low_n = c("Morocco")
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
    "Other" = c("Other", "Other Chinese dialects", "Other local; aboriginal; tribal, community", "Other Oceania")
  ),
  Religion = list(
    "Other" = c("Other; nfd", "Noneligious", "Non classified christian movements; nfd")
  ),
  Ethnicity = list(
    "Other" = c("Other, Non-Hispanic", "Two plus, non-Hispanic")
  )
)