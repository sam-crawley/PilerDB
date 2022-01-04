data.spec <- list(
  file.name = "Divided/datasets/cses/IMD/cses_imd.dta",
  file.type = 'dta',
  skip.countries = list(
    no_group = c("Denmark", "Lithuania", "Italy")
  ),
  wave_var = "module",
  split.by.year = T,
  country.format = 'country.name',
  question.text = c(
    "Party" = "Which party do you feel closest to?",
    "Language" = "Language usually spoken at home",
    "Religion" = "Religious denomination",
    "Ethnicity" = "Race of respondent"
  ),
  field.def = c(
    "Party" = "IMD3005_3",
    "Language" = "IMD2013",
    "Religion" = "IMD2005",
    "Ethnicity" = "IMD2010",
    "Weight" = "IMD1010_1",
    "Country" = "IMD1006_NAM",
    "Year" = "IMD1008_YEAR"
  ),
  fixups = function(data) {
    # Strip out country prefixes from levels
    levels(data$Party) <- str_remove(levels(data$Party), "^\\d+\\. \\w+ - \\s*")
    
    for (var in c("Language", "Religion", "Ethnicity")) {
      levels(data[[var]]) <- str_remove(levels(data[[var]]), "^\\d+\\. ")
    }
    
    data <- data %>% mutate(module = case_when(
      IMD1008_MOD_1 == 1 ~ 1,
      IMD1008_MOD_2 == 1 ~ 2,
      IMD1008_MOD_3 == 1 ~ 3,
      IMD1008_MOD_4 == 1 ~ 4
    ))
    
    # Fix CSES coding error - Thailand respondents coded as "ESAN" rather than "ISAN"
    data <- data %>%
      mutate(Language = replace(Language, Country == "Thailand" & Language == "ESAN (NIGERIA)", "ISAN THAI"))

    data
  }  
)

cat.defs <- list(
  Party = list(
    "Missing" = c("9999997. VOLUNTEERED: REFUSED", "9999998. VOLUNTEERED: DON'T KNOW", "9999999. MISSING", "9999988. NONE OF THE CANDIDATES PARTIES"),
    "Other" = c("9999989. INDEPENDENT CANDIDATE",  "9999992. OTHER CANDIDATE/PARTY (NOT FURTHER SPECIFIED)", "Others")
  ),
  Language = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("OTHER: NOT SPECIFIED", "OTHER: TWO OR MORE LANGUAGES", "OTHER: LOCAL DIALECT")
  ),
  Religion = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("AGNOSTICS", "NON-BELIEVERS", "OTHER: NOT SPECIFIED")
  ),
  Ethnicity = list(
    "Missing" = c("VOLUNTEERED: REFUSED", "VOLUNTEERED: DON'T KNOW", "MISSING"),
    "Other" = c("OTHER: NOT SPECIFIED")
  )
)
