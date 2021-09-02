data.spec <- list(
  file.name = "Divided/data/evs/2017/ZA7500_v4-0-0.dta",
  file.type = 'dta',
  skip.countries = list(
    no_group = c("Armenia", "Azerbaijan", "Croatia", "Czechia", "Denmark", "Finland", "Italy", "Poland", "Portugal", "Serbia")
  ),
  country.format = 'iso2c',
  field.def = c(
    "Party" = "v174_cs",
    "Language" = NA,
    "Religion" = "v52_cs",
    "Ethnicity" = NA,
    "Weight" = "dweight",
    "Country" = "c_abrv",
    "Year" = "year"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    for (var in main.vars) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\s*\\w+:\\s*")
    }
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("not applicable", "no answer", "dont know"),
    "Other" = c("No (other) party appeals to me (spontaneous)", "No [no other] party appeals to me (spontaneous)", "No [,no other] party appeals to me",
                "No [,no other] party appeals to me (spontaneous)", "No [, no other] party appeals to me (spontaneous)", "Other", "Other, please specify (WRITE IN)",
                "No [no other] party appeals to me", "Other Left Wing Parties (Radical Leftist Party, Republicain's and Citizen's Movement", "Other Environmentalist Parties",
                "Other Right Wing Extremist Parties (The Patriots, National Republican Movement)", "No (no other) party appeals to me", "No [, no other] party appeals to me",
                "Other, please specify (WRITE IN):", "Other party (WRITE IN)", "No party appeals to me (spontaneous)")
  ),
  Religion = list(
    "Missing" = c("not applicable", "no answer", "dont know"),
    "Other" = c("Other, please specify (Write in)", "Other than Christian", "Other", "Other religious denomination, please specify (Write in)", "Other, which?",
                "Other Non Christian", "Other, Non-Christian Religion", "Other non-Christian (WRITE IN)")
  )
)
