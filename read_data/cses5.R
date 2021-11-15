data.spec <- list(
  file.name = "Divided/datasets/cses/Module 5/cses5.dta",
  file.type = 'dta',
  skip.countries = list(),
  split.by.year = T,
  country.format = 'country.name',
  question.text = c(
    "Party" = "Which party do you feel closest to?",
    "Language" = "Language usually spoken at home",
    "Religion" = "Religious denomination"
  ),  
  field.def = c(
    "Party" = "E3024_3",
    "Language" = "E2019",
    "Religion" = "E2013",
    "Ethnicity" = NA,
    "Weight" = "E1010_2",
    "Country" = "E1006_NAM",
    "Year" = "E1008"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    levels(data$Party) <- str_remove(levels(data$Party), "^\\d+\\. \\w+ - \\s*")
    
    for (var in c("Language", "Religion")) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\d+\\. ")
    }
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("999997. VOLUNTEERED: REFUSED", "999998. VOLUNTEERED: DON'T KNOW", "999999. MISSING"),
    "Other" = c("999989. INDEPENDENT CANDIDATE", "999992. OTHER CANDIDATE/PARTY (NOT FURTHER SPECIFIED)")
  ),
  Language = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("[SEE ELECTION STUDY NOTES]", "OTHER: NOT SPECIFIED")
  ),
  Religion = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("INDEPENDENT, OTHER [SEE ELECTION STUDY NOTES]", "AGNOSTIC", "ATHEIST", "NONE",
                "[SEE ELECTION STUDY NOTES]", "OTHER: NOT SPECIFIED")
  )
)
