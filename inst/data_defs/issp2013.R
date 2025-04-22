data.spec <- list(
  file.name = "ZA5950_v2-0-0.dta",
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
    "BE-BRU" = "Belgium", 
    "BE-FLA" = "Belgium",
    "BE-WAL" = "Belgium", 
    "DE-E" = "Germany",
    "DE-W" = "Germany",
    "IL-A" = "Israel",
    "IL-J" = "Israel"
  ),
  field.def = c(
    "Party" = "prty",
    "Religion" = "relig",
    "Language" = NA,
    "Ethnicity" = "ethn",
    "Country" = "C_ALPHAN",
    "Year" = "DATEYR",
    "Weight" = "WEIGHT"
  ),
  pre_fixups = function(data) {
    data <- data %>%
      mutate(across(c(ends_with("PRTY"), ends_with("RELIG")), ~if_else(.x == 990, NA, .x))) %>%
      mutate(across(ends_with("ETHN1"), ~if_else(.x == 0, NA, .x)))
    
    # Coalece necessary vars
    data <- coalese.vars(data, str_subset(names(data), "PRTY$"), "prty")
    data <- coalese.vars(data, str_subset(names(data), "RELIG$"), "relig")
    data <- coalese.vars(data, str_subset(names(data), "ETHN1$"), "ethn")
    
    # Handle missing (etc.) based on response code
    data <- data %>%
      mutate(prty = if_else(str_detect(prty, "^NAP"), "Missing", prty)) %>%
      mutate(DATEYR = if_else(DATEYR == 9999, NA, DATEYR))
    
    data
  },
  fixups = function(data) {
    # Remove unusable ethnicity questions
    data %>% mutate(Ethnicity = if_else(Country %in% c("Iceland", "Mexico"), "Missing", Ethnicity))
  } 
)

cat.defs <- list(
  Party = list(
    "Missing" = c("Invalid ballot", "Refused", "No answer", "Don't know, don't remember",
                  "Don't Know", "Don't remember", "No party affiliation", "Don't know",
                  "Can't say", "DK/Refused", "Uncertain", "Did not vote", "Dont know",
                  "Invalid ballot, Vote blank", "Vote blank / Invalid ballot"),
    "Other" = c("Other party", "Other Party", "Mixed vote (Candidates of several lists)",
                "Other party voted", "Other - Multiple parties selected",
                "Independent", "Other (specify)", "Other Candidate", "Other answer")
  ),
  Religion = list(
    "Missing" = c("Refused", "No answer", "Don't know", "DK/Refused", "Don\"t know"),
    "Other" = c("Other", "Other religions", "Other Asian religions",
                "Other religious denomination", "Other religion", "Other religion",
                "Other Religions", "Other Non-Christians", "Other non-Christian Religions",
                "Other Religion", "Another non-Christian religion", "Other (i.e. Folk religion)",
                "Something else", "Other Non-Christian (Hindu, Hare Krishna)",
                "Other non-Christian", "Other not Christian"),
    "No Religion" = c("No denomination or life stance organisation")
  ),
  Ethnicity = list(
    "Missing" = c("No answer", "Don't know", "Refused", "Dont know", "Undecided",
                  "No answer/ Inadequately described", "No ethnic group"),
    "Other" = c("Other, please specify", "Immigrants", "Others", "0ther",
                "3 ethnic groups or more", "Other nationa/ ethnic identity",
                "Other (write in)", "Other ethnic group")
  )
)