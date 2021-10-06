data.spec <- list(
  file.name = "Divided/datasets/cses/IMD/cses_imd.dta",
  file.type = 'dta',
  skip.countries = list(
    no_group = c("Denmark", "Lithuania")
  ),
  wave_var = "module",
  country.format = 'country.name',
  field.def = c(
    "Party" = "IMD3005_3",
    "Language" = "IMD2013",
    "Religion" = "IMD2005",
    "Ethnicity" = "IMD2010",
    "Weight" = "IMD1010_1",
    "Country" = "IMD1006_NAM",
    "Year" = "IMD1013_Y"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    #levels(data$Party) <- str_remove(levels(data$Party), "^\\d+\\. \\w+ - \\s*")
    
    for (var in c("Language", "Religion")) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\d+\\. ")
    }
    
    data <- data %>% mutate(module = case_when(
      IMD1008_MOD_1 == 1 ~ 1,
      IMD1008_MOD_2 == 1 ~ 2,
      IMD1008_MOD_3 == 1 ~ 3,
      IMD1008_MOD_4 == 1 ~ 4
    ))
    
    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("999997. VOLUNTEERED: REFUSED", "999998. VOLUNTEERED: DON'T KNOW", "999999. MISSING"),
    "Other" = c("999989. INDEPENDENT CANDIDATE", "999990. OTHER LEFT WING CANDIDATE/PARTY (NOT FURTHER SPECIFIED)", 
                "999991. OTHER RIGHT WING CANDIDATE/PARTY (NOT FURTHER SPECIFIED)", "999992. OTHER CANDIDATE/PARTY (NOT FURTHER SPECIFIED)")
  ),
  Language = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("[SEE ELECTION STUDY NOTES]", "OTHER: NOT SPECIFIED")
  ),
  Religion = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("INDEPENDENT, OTHER [SEE ELECTION STUDY NOTES]", "AGNOSTIC", "ATHEIST", "EHTNORELIGIONIST, OTHER [SEE ELECTION STUDY NOTES]", "NONE",
                "[SEE ELECTION STUDY NOTES]", "OTHER: NOT SPECIFIED")
  )
)
