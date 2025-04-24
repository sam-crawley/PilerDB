data.spec <- list(
  file.name = "ZA4350_v2-0-0.dta",
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
    "BE-FLA" = "Belgium",
    "DE-E" = "Germany",
    "DE-W" = "Germany"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "RELIG",
    "Language" = NA,
    "Ethnicity" = "ETHNIC",
    "Country" = "C_ALPHAN",
    "Year" = "Year",
    "Weight" = "WEIGHT"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY")), ~if_else(.x == 0, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(Year = 2005) %>%
      mutate(prty = if_else(C_ALPHAN == "US", "Missing", prty))
    
    data
  },
  fixups = function(data) {
    data %>%
      mutate(across(c(Party, Religion, Ethnicity), ~str_replace(.x, "^\\d+\\.\\s+", "")))
  }
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Uncertain", "No party preference", "Would not vote", "Don't support any party",
                   "None", "Will not vote", "No party", "No party affiliation",
                   "Would not vote,no preference", "Would not vote; no party preference",
                  "None/No party preference", "Would not vote;no party preference",
                  "No preference", "Didn't vote/not eligible", "Would not vote;not eligible",
                  "Blank vote", "No party preference;wouldn't vote",
                  "Would not vote;No party preference"),
    "Other" = c("Other party", "Other answer", "Other Party", "Other party, specify",
                "Other Parties", "Other responses")
  ),
  Religion = list(
    "Missing" = c("Don't know", "No answer, NL: NAV"),
    "Other" = c("Other Religions"),
    "No Religion" = c("No religion")
  ),
  Ethnicity = list(
    "Missing" = c("NAP; NAV", "NA, Don't know"),
    "Other" = c("Other,mixed origin")
  )
)