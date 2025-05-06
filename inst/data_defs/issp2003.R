data.spec <- list(
  file.name = "ZA3910_v2-1-0.dta",
  file.type = 'dta',
  file.encoding = "latin1",
  question.text = c(
    "Party" = "Which party did you vote for?",
    "Religion" = "Do you belong to a religion and, if yes, which religion do you belong to?",
    "Ethnicity" = "Nationality / ethnic group"
  ),
  party.question.type = "PartyVote",
  country.format = 'iso2c',
  country.custom = c(
    "GB-GBN" = "United Kingdom",
    "DE-E" = "Germany",
    "DE-W" = "Germany",
    "IL-A" = "Israel",
    "IL-J" = "Israel"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = "ethnic",
    "Country" = "C_ALPHAN",
    "Year" = "Year",
    "Weight" = "weight"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("prty")), ~if_else(.x == 0, NA, .x)))
    
    # Coalesce necessary vars
    data <- coalese.vars(data, str_subset(names(data), "prty$"), "prty")
    
    data <- data %>%
      mutate(Year = 2003) %>%
      mutate(prty = if_else(C_ALPHAN == "US", "Missing", prty))
    
    data
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c( "No party preference", "Would not vote", "No party affiliation",
                   "None",  "Would not vote;no party preference", "No vote",
                   "None, no preference", "No party, no preference", "Would not vote;not eligible",
                   "No preference", "Abstain, no party preference", "Would not vote;No party preference",
                   "No vote (No votar\u00e1)", "No party preference;wouldn't vote", "White (En Blanco)",
                   "No preference,no vote", "(Undocumented)"),
    "Other" = c("Other party", "Other answer", "Other Party", "Other list", "Others",
                "Other Party (Otros)")
  ),
  Religion = list(
    "Missing" = c("Don't know"),
    "Other" = c("Other Religions"),
    "No Religion" = c("No religion")
  ),
  Ethnicity = list(
    "Missing" = c("NAV", "No answer,Don't know", "No ethnic identity at all"),
    "Other" = c("One non-Swedish,both non-Swedish", "Other answers",
                "Other,Mixed origin")
  )
)