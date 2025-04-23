data.spec <- list(
  file.name = "ZA5400_v4-0-0.dta",
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
    "BE-FLA" = "Belgium"
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
      mutate(prty = if_else(str_detect(prty, "^NAP"), "Missing", prty)) %>%
      mutate(Year = 2009) %>%
      mutate(prty = if_else(C_ALPHAN == "US", "Missing", prty))
    
    data
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Refused", "No party preference", "No party affiliation", 
                  "Not interested so much in politics", "Not eligible", "Did not vote",
                  "Did not vote/ not eligible (code 2 in VOTE_LE)",
                  "Would not vote; No party preference", "DK", "NA",
                  "None; No party preference", "No party, no preference",
                  "Vote blanc", "Novoto vreme", "No party; No party preference",
                  "Blank vote (no mark)", "Would not vote (abstention); No party preference",
                  "Blank vote (invalid)", "Uncertain", "None, does not identify with any party",
                  "Vote for a person not for a party"),
    "Other" = c("Other party", "Independent", "Other answer", "Other regional party")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer; Refused (DE,EE,ZA,RU); DK/NA (ES)",
                  "Dont know"),
    "Other" = c("Other Religions", "Other non-Christian Religions",
                "IS: More than one religion"),
    "No Religion" = c("No religion")
  ),
  Ethnicity = list(
    "Missing" = c("NAP; NAV (AU,CH,CL,CZ,DK,ES,FR,GB-GBN,HR,IL,IT,NO,PL,PT,RU)",
                  "NA, don't know"),
    "Other" = c("34.1", "82.2", "82.1", "34.2")
  )
)